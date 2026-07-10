# Process Amacayacu plot data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-25

# Import data
s <- readRDS(file.path(indir, "amacayacu_census4_20260225.rds"))
q <- read.csv(file.path(indir, "quadrat.20260224.csv"))
sp <- read.csv(file.path(indir, "species.20260224.csv"))
p <- read_excel(file.path(indir, "plot_pi.xlsx"))
poly <- st_read(file.path(indir, "poly/Amacayacu_plot_new.shp"))

# Process plot corners
ptc <- st_coordinates(poly) 
crs <- unique(getUTM(ptc[,1], ptc[,2], epsg = TRUE))
crs_name <- paste("UTM", unique(getUTM(ptc[,1], ptc[,2], epsg = FALSE)))

pt <- poly %>% 
  st_set_crs(., 4326) %>% 
  st_cast("POINT") %>% 
  slice_tail(n = -1) %>% 
  mutate(point_id = c("SW", "NW", "NE", "SE")) %>%
  st_transform(., crs = crs) %>% 
  bind_cols(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  rename(
    rover_easting_utm_m = X,
    rover_northing_utm_m = Y) %>% 
  mutate(
    site_id = param$site_id, 
    acquisition_id = param$acquisition_id, 
    plot_id = "Amacayacu_1",
    x_rel_m = case_when(
      point_id %in% c("SW", "NW") ~ 0,
      point_id %in% c("SE", "NE") ~ 500,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      point_id %in% c("SW", "SE") ~ 0,
      point_id %in% c("NW", "NE") ~ 500,
      TRUE ~ NA_real_),
    crs_epsg = as.integer(crs),
    crs_name = crs_name,
    rover_model = "Garmin GPSMap 65s", 
    corner = TRUE) %>% 
  colGen(., pt_cols$column_name, pt_cols$class) %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Process stem data
s_clean <- s %>% 
  left_join(., sp, by = "spcode") %>% 
  rename(
    tree_id = tag,
    stem_id = stemtag,
    taxon_name = species,
    x_rel_m = gx,
    y_rel_m = gy,
    subplot_id = quadrat, 
    pom_m = hom4,
    measurement_date = date4,
    notes = obs4) %>% 
  mutate(
    acquisition_id = param$acquisition_id,
    site_id = param$site_id, 
    census_id = "4",
    plot_id = "Amacayacu_1",
    diam_cm = dbh4 / 10,
    height_m = NA_real_,
    measurement_date = as.character(measurement_date),
    alive = ifelse(status4 %in% c("alive", "P"), "A", "D"),
    fallen = "S",
    broken = ifelse(grepl("Q", codes4), "B", ""),
    missing = ifelse(grepl("DD", codes4), "M", ""),
    code = pasteVals(alive, broken, fallen, missing),
    agb_allometry = NA_character_,
    growth_form = NA_character_,
    height_allometry = NA_character_) %>% 
  group_by(plot_id) %>% 
  mutate(census_date = as.character(median(as.Date(measurement_date)))) %>% 
  ungroup() %>% 
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Create plot metadata table
p_clean <- data.frame(
  site_id = param$site_id, 
  acquisition_id = param$acquisition_id,
  plot_id = "Amacayacu_1",
  census_date = unique(s_clean$census_date),
  census_id = "4",
  plot_width_m = 500,
  plot_length_m = 500,
  plot_slope_deg = NA_real_,
  plot_aspect_deg = NA_real_,
  plot_elevation_m = NA_real_,
  plot_planar = TRUE,
  notes_plot = NA_character_,
  meas_diam_min_cm = 10,
  meas_pom_default_m = 1.3,
  meas_tree_stem = TRUE,
  meas_tree_group = TRUE,
  meas_dead = FALSE,
  meas_fallen = TRUE,
  meas_liana = NA,
  meas_palm = NA,
  meas_bamboo = NA,
  meas_protocol = NA_character_,
  meas_plot_loc = NA_character_,
  meas_stem_loc = "ForestGEO protocol. Well-surveyed 10x10 m subplot grid. Tape measures to locate stems by X and Y coordinates.",
  notes_meas = NA_character_,
  forest_status = "old-growth",
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

# Check all columns in output objects
colCheck(p_clean, plot_cols)
colCheck(pt, pt_cols)
colCheck(s_clean, stem_cols)

# Check values
valCheck(
  plot = p_clean,
  stem = s_clean,
  pt = pt)

# Write corner points to file
write.csv(pt, file.path(outdir, "plot_pt.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(p_clean, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
