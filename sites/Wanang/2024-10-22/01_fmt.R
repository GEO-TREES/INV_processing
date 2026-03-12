# Format Wanang tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-18

# Import data
pt <- read.csv(file.path(indir, "plot_corners.csv"))
s <- read.csv(file.path(indir, "s.csv"))

# Format stem data
s_clean <- s %>%
  rename(
    subplot_id = QuadratName,
    x_rel_m = QX_plot,
    y_rel_m = QY_plot,
    stem_id = StemID,
    diam_cm = DBH,
    pom_m = HOM, 
    measurement_date = EaxctDate) %>%
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = as.character(plot_id),
    subplot_id = as.character(subplot_id),
    stem_id = as.character(stem_id),
    record_id = row_number(),
    tree_id = NA_character_,
    height_m = NA_real_,
    alive = ifelse(status %in% c("alive", "alivealive below"), "A", "D"),
    missing = ifelse(status %in% c("missing") & is.na(diam_cm), "M", ""),
    broken = "",
    stump = "", 
    fallen = "S", 
    code = pasteVals(alive, fallen, broken, missing, stump),
    agb_allometry = NA_character_,
    notes = NA_character_,
    measurement_date = ifelse(is.na(measurement_date), first(na.omit(measurement_date)), measurement_date),
    taxon_name = paste(Genus, SpeciesName)) %>% 
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  group_by(plot_id) %>% 
  mutate(census_date = as.character(median(as.Date(measurement_date)))) %>% 
  ungroup() %>%
  dplyr::select(all_of(stem_cols$column_name))

# Format plot corners
pt_clean <- pt %>% 
  rename(point_id = corner_id) %>% 
  mutate(plot_id = as.character(plot_id)) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 32755) %>% 
  st_transform(., 4326) %>% 
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id
    ) %>% 
  mutate(
         x_rel_m = as.numeric(x_rel_m),
         y_rel_m = as.numeric(y_rel_m)) %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Create plot meta-data table
plots <- s_clean %>% 
  dplyr::select(site_id, acquisition_id, plot_id, census_date) %>% 
  distinct() %>% 
  mutate(
    min_diam_thresh_cm = 10,
    plot_width_m = 100,
    plot_length_m = case_when(
      plot_id == "2" ~ 400,
      TRUE ~ 500),
    plot_slope_deg = NA_real_,
    plot_aspect_deg = NA_real_,
    plot_elevation_m = NA_real_,
    plot_planar = TRUE,
    notes_plot = NA_character_,
    meas_diam_min_cm = 10,
    meas_pom_default_m = 1.3,
    meas_tree_stem = FALSE,
    meas_tree_group = FALSE,
    meas_dead = TRUE,
    meas_fallen = TRUE,
    meas_liana = TRUE,
    meas_palm = TRUE,
    meas_bamboo = TRUE,
    meas_protocol = NA_character_,
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
colCheck(pt_clean, pt_cols)
colCheck(s_clean, stem_cols)

# Check values
valCheck(
  plot = plots,
  stem = s_clean,
  pt = pt_clean)

# Write corner points to file
st_write(pt_clean, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write plot meta-data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
    
