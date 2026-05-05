# Clean Bicuar tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-03-03

# Import data
s <- read.csv(file.path(indir, "stems.csv"))
p <- read.csv(file.path(indir, "plots.csv"))
wd <- read.csv(file.path(indir, "wood_density_raw.csv"))
plot_corners <- read_sf(file.path(indir, "plot_corners.shp"))

# Process plot corners
pt <- plot_corners %>% 
  mutate(
    corner = TRUE,
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    point_id = gsub(".*[0-9]+", "", name)) %>%
  rename(plot_id = plot_name) %>% 
  mutate(
    x_rel_m = case_when(
      point_id %in% c("SW", "NW") ~ 0,
      point_id %in% c("SE", "NE") ~ 100,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      point_id %in% c("SW", "SE") ~ 0,
      point_id %in% c("NW", "NE") ~ 100,
      TRUE ~ NA_real_)) %>% 
  filter(plot_id != "P1")

ptc <- st_coordinates(pt) 
crs <- unique(getUTM(ptc[,1], ptc[,2], epsg = TRUE))
crs_name <- unique(getUTM(ptc[,1], ptc[,2], epsg = FALSE))

pt_clean <- pt %>% 
  filter(plot_id != "P16") %>% 
  st_transform(., crs = crs) %>% 
  bind_cols(., st_coordinates(.)) %>% 
  rename(
    rover_easting_utm_m = X,
    rover_northing_utm_m = Y) %>% 
  mutate(
    crs_epsg = as.integer(crs),
    crs_name = "UTM 33S",
    rover_model = "Garmin GPSMap 65s") %>% 
  colGen(., pt_cols$column_name, pt_cols$class) %>% 
  st_drop_geometry() %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Prepare stem data 
s_clean <- s %>% 
  left_join(., unique(p[,c("plot_id", "plot_name")]), by = "plot_id") %>% 
  group_by(plot_name, stem_id) %>% 
  arrange(census_date) %>% 
  fill(x_grid, y_grid, subplot_id, .direction = "downup") %>% 
  ungroup() %>% 
  dplyr::select(-plot_id) %>% 
  rename(
    measurement_date = census_date,
    plot_id = plot_name,
    x_rel_m = x_grid,
    y_rel_m = y_grid,
    diam_cm = diam,
    pom_m = pom,
    height_m = height, 
    taxon_name = species_name_clean,
    notes = notes_stem) %>% 
  mutate(
    acquisition_id = param$acquisition_id,
    site_id = param$site_id,
    alive = ifelse(stem_status %in% c("a", "r"), "A", "D"),
    broken = ifelse(grepl("b|p", stem_mode), "B", ""),
    fallen = ifelse(grepl("f", stem_mode), "F", "S"),
    missing = ifelse(grepl("v|q", stem_mode), "M", ""),
    stump = ifelse(grepl("t", stem_mode), "T", ""),
    code = pasteVals(alive, broken, fallen, missing),
    growth_form = NA_character_,
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    subplot_id = as.character(subplot_id)) %>% 
  mutate(
    year = gsub("-.*", "", measurement_date),
    census_id = case_when(
      plot_id %in% paste0("P", 1:4) & year == "2018" ~ "1",
      plot_id %in% paste0("P", 2:4) & year == "2021" ~ "2",
      plot_id %in% paste0("P", 2:4) & year == "2024" ~ "3",
      plot_id %in% paste0("P", 5:15) & year == "2019" ~ "1",
      plot_id %in% paste0("P", 5:15) & year == "2021" ~ "2",
      plot_id %in% paste0("P", 5:15) & year == "2024" ~ "3",
      plot_id %in% paste0("P", 16) & year == "2021" ~ "1",
      plot_id %in% paste0("P", 16) & year == "2024" ~ "2",
      plot_id %in% paste0("M", 1:3) & year == "2022" ~ "1",
      plot_id %in% paste0("M", 1:3) & year == "2024" ~ "2",
      plot_id %in% paste0("O", 1:2) & year == "2024" ~ "1",
      plot_id %in% paste0("B", 1:2) & year == "2024" ~ "1",
      TRUE ~ NA_character_)) %>% 
  group_by(plot_id, census_id) %>% 
  mutate(census_date = as.character(median(as.Date(measurement_date)))) %>% 
  ungroup() %>% 
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() 

s_out <- s_clean %>% 
  mutate(record_id = row_number()) %>% 
  filter(
    plot_id != "P16",
    grepl("2024", census_date)) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Create optional height measurements table
s_height <- s_clean %>% 
  filter(
    !is.na(diam_cm), !is.na(height_m),
    !grepl("D", code),
    !grepl("B", code),
    !grepl("F", code),
    !grepl("M", code),
    !grepl("T", code)) %>% 
dplyr::select(all_of(height_cols$column_name))
  
# Create plots table
plots <- p %>% 
  dplyr::select(-plot_id) %>% 
  rename(plot_id = plot_name) %>% 
  mutate(site_id = param$site_id) %>% 
  filter(
    grepl("2024", census_date),
    !plot_id %in% c("P1", "P16")) %>% 
  mutate(
    acquisition_id = param$acquisition_id,
    census_id = "3",
    plot_width_m = 100,
    plot_length_m = 100,
    plot_slope_deg = NA_real_,
    plot_aspect_deg = NA_real_,
    plot_elevation_m = NA_real_,
    plot_planar = FALSE,
    notes_plot = NA_character_,
    meas_diam_min_cm = 5,
    meas_pom_default_m = 1.3,
    meas_tree_stem = TRUE,
    meas_tree_group = TRUE,
    meas_dead = TRUE,
    meas_fallen = TRUE,
    meas_liana = NA,
    meas_palm = NA,
    meas_bamboo = NA,
    meas_plot_loc = NA_character_,
    meas_stem_loc = NA_character_,
    meas_protocol = "SEOSAW_v3.6",
    notes_meas = NA_character_,
    forest_status = NA_character_,
    vegetation_type = NA_character_,
    land_use = NA_character_,
    treatment = NA_character_,
    treatment_ref = NA_character_,
    fire_regime = NA_character_,
    cyclone_regime = NA_character_,
    flood_regime = NA_character_,
    earth_regime = NA_character_,
    herbivory_regime = NA_character_,
    notes_disturbance = NA_character_) %>% 
  dplyr::select(all_of(plot_cols$column_name))

# Process wood density data
wd_clean <- wd %>% 
  mutate(site_id = param$site_id) %>% 
  rename(
    taxon_name = species,
    wood_density_gcm3 = WD) %>% 
  mutate(
    wood_density_sd_gcm3 = NA_real_,
    wood_density_n = as.integer(NA_real_)) %>%
  filter(!is.na(taxon_name), !is.na(wood_density_gcm3)) %>% 
  dplyr::select(all_of(wd_cols$column_name))


# Check all columns in output objects
colCheck(plots, plot_cols)
colCheck(pt_clean, pt_cols)
colCheck(s_out, stem_cols)
colCheck(wd_clean, wd_cols)
colCheck(s_height, height_cols)

# Check values
valCheck(
  plot = plots, 
  stem = s_out, 
  wd = wd_clean,
  pt = pt_clean,
  height = s_height)

# Write corner points to file
write.csv(pt_clean, file.path(outdir, "plot_pt.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_out, file.path(outdir, "stem.csv"), row.names = FALSE)

# Write height data to file
write.csv(s_height, file.path(outdir, "height.csv"), row.names = FALSE)

# Write wood density data to file
write.csv(wd_clean, file.path(outdir, "wd.csv"), row.names = FALSE)

