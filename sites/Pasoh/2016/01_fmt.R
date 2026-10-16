# Clean Pasoh stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-05-12

# Import stem data
load(file.path(indir, "pasoh.stem7.rdata"))  # TODO: replace with 2024 census
# pasoh.stem7

# Import species table
load(file.path(indir, "pasoh.spptable.rdata"))
# pasoh.spptable

# Import elevation raster 
elev <- rast(file.path(indir, "elev_rast_legacy.tif"))

# Format stem data
s_clean <- pasoh.stem7 %>% 
  left_join(., pasoh.spptable, by = c("SP" = "sp")) %>% 
  rename(
    census_id = CensusID,
    subplot_id = Quadrat,
    tree_id = TreeID,
    stem_id = StemID,
    measurement_date = ExactDate,
    taxon_name = Latin,
    x_rel_m = PX,
    y_rel_m = PY,
    diam_cm = DBH,
    pom_m = HOM) %>% 
  mutate(
    site_id = param$site_id,
    plot_id = "Pasoh",
    acquisition_id = "2016-08-15",
    measurement_date = as.Date(measurement_date),
    census_date = as.character(mean(measurement_date, na.rm = TRUE)),
    measurement_date = as.character(measurement_date),
    census_id = as.character(census_id),
    tree_id = as.character(tree_id),
    stem_id = as.character(stem_id),
    diam_cm = as.numeric(diam_cm) / 10,
    x_rel_m = as.numeric(x_rel_m),
    y_rel_m = as.numeric(y_rel_m),
    pom_m = as.numeric(pom_m),
    pom_m = ifelse(pom_m == 0, NA_real_, pom_m),
    subplot_id = as.character(subplot_id),
    taxon_name = paste0(
      toupper(substr(tolower(taxon_name), 1, 1)), 
      substr(tolower(taxon_name), 2, nchar(taxon_name))
    ),
    fallen = ifelse(grepl("Y", Codes), "F", "S"),
    broken = ifelse(DFstatus == "broken below" | grepl("R", Codes), "B", ""),
    missing = ifelse(grepl("DD", Codes), "M", ""),
    code = pasteVals(Status, fallen, broken, missing)) %>%  # TODO: Double check
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(record_id = row_number()) %>% 
  colGen(., stem_cols$column_name, stem_cols$class) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Clean plot coordinates
plot_pt <- st_as_sfc(st_bbox(elev)) %>% 
  st_cast(., "POINT") %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  slice_head(n = 4) %>% 
  rename(
    rover_easting_utm_m = X,
    rover_northing_utm_m = Y) %>% 
  mutate(
    site_id = param$site_id,
    plot_id = "Pasoh",
    point_id = c("SW", "SE", "NE", "NW"),
    corner = TRUE,
    acquisition_id = "2016-08-15",
    x_rel_m = case_when(
      point_id == "SW" ~ 0,
      point_id == "SE" ~ 1000,
      point_id == "NE" ~ 1000,
      point_id == "NW" ~ 0,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      point_id == "SW" ~ 0,
      point_id == "SE" ~ 0,
      point_id == "NE" ~ 500,
      point_id == "NW" ~ 500,
      TRUE ~ NA_real_),
    crs_epsg = as.integer(32648),
    crs_name = "UTM 48N") %>% 
  colGen(., pt_cols$column_name, pt_cols$class) %>% 
  dplyr::select(all_of(pt_cols$column_name))


# Clean plot meta-data
plot_meta <- data.frame(
  site_id = param$site_id,
  acquisition_id = "2016-08-15",
  plot_id = "Pasoh",
  census_id = unique(s_clean$census_id),
  census_date = unique(s_clean$census_date),
  plot_planar = TRUE,
  plot_length_m = 1000,
  plot_width_m = 500,
  meas_diam_min_cm = 10,
  meas_pom_default_m = 1.3,
  meas_tree_stem = TRUE,
  meas_tree_group = TRUE,
  meas_dead = FALSE,
  meas_fallen = FALSE,
  meas_liana = NA,
  meas_palm = NA,
  meas_bamboo = NA,
  meas_plot_loc = NA_character_,
  meas_stem_loc = "ForestGEO protocol. Well-surveyed 10x10 m subplot grid. Tape measures to locate stems by X and Y coordinates.",
  meas_protocol = NA_character_,
  notes_meas = NA_character_,
  forest_status = "old-growth",
  vegetation_type = NA_character_,
  land_use = NA_character_,
  treatment = NA_character_,
  fire_regime = NA_character_,
  cyclone_regime = NA_character_,
  flood_regime = NA_character_,
  earth_regime = NA_character_,
  herbivory_regime = NA_character_,
  notes_disturbance = NA_character_,
  notes_plot = NA_character_) %>% 
  dplyr::select(all_of(plot_cols$column_name))

# Check all columns in output objects
colCheck(plot_meta, plot_cols)
colCheck(plot_pt, pt_cols)
colCheck(s_clean, stem_cols)

# Check values
valCheck(
  plot = plot_meta,
  stem = s_clean,
  pt = plot_pt)

# Write corner points to file
write.csv(plot_pt, file.path(outdir, "plot_pt.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(plot_meta, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
