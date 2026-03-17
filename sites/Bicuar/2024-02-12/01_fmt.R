# Clean Bicuar tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-03-03

# Import data
s <- read.csv(file.path(indir, "stems.csv"))
p <- read.csv(file.path(indir, "plots.csv"))
plot_corners <- read_sf(file.path(indir, "plot_corners.shp"))

# Process plot corners
pt <- plot_corners %>% 
  mutate(
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
  filter(plot_id != "P1") %>% 
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
    code = pasteVals(alive, broken, fallen, missing, stump),
    agb_allometry = NA_character_,
    subplot_id = as.character(subplot_id)) %>% 
  group_by(plot_id) %>% 
  mutate(census_date = as.character(median(as.Date(measurement_date)))) %>% 
  ungroup() %>% 
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() 

s_out <- s_clean %>% 
  mutate(record_id = row_number()) %>% 
  filter(grepl("2024", census_date)) %>% 
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
dplyr::select(
  site_id,
  plot_id, 
  diam_cm,
  height_m)
  
# Create plots table
plots <- p %>% 
  dplyr::select(-plot_id) %>% 
  rename(plot_id = plot_name) %>% 
  mutate(site_id = param$site_id) %>% 
  filter(grepl("2024", census_date)) %>% 
  filter(plot_id != "P1") %>% 
  mutate(
    acquisition_id = param$acquisition_id,
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
    meas_protocol = "SEOSAW_v3.6",
    notes_meas = NA_character_,
    forest_status = NA_character_,
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

# Check all columns in output objects
colCheck(plots, plot_cols)
colCheck(pt, pt_cols)
colCheck(s_out, stem_cols)

# Check values
valCheck(
  plot = plots, 
  stem = s_out, 
  pt = pt)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write plot meta-data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_out, file.path(outdir, "stem.csv"), row.names = FALSE)

# Write height data to file
write.csv(s_height, file.path(outdir, "stem_height.csv"), row.names = FALSE)

