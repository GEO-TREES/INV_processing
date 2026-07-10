# Clean Mpala tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-06-14

# Import data
s <- read.csv(file.path(indir, "Census Tree_0.csv"))
s_mult <- read.csv(file.path(indir, "Multiples_1.csv"))
taxa <- read_excel(file.path(indir, "Mpalaspplist_16Jul2016.xlsx"))
pt_raw <- st_read(file.path(indir, "Points.shp"))

# Create sf polygon object
pt <- pt_raw %>% 
  rename(
    point_id = Name,
    rover_easting_utm_m = Easting,
    rover_northing_utm_m = Northing,
    rover_elevation_m = Elevation,
    rover_ellipse_height_m = Ellips.ht,
    rover_easting_rms_m = RMS.E,
    rover_northing_rms_m = RMS.N,
    rover_elevation_rms_m = Elev.RMS,
    rover_lateral_rms_m = Later.RMS,
    rover_antenna_height_m = Antenna.ht,
    rover_start_time = Avg.start,
    rover_end_time = Avg.end,
    rover_sample_n = Samples,
    rover_model = Dev.type,
    rover_gdop = GDOP,
    base_easting_utm_m = Base.E,
    base_northing_utm_m = Base.N,
    base_elevation_m = Base.elev,
    rover_solution_status = Solution
  ) %>% 
  mutate(
    rover_pdop = NA_real_,
    base_ellipse_height_m = NA_real_,
    rover_satellite_n = as.numeric(GPS) + as.numeric(GLONASS) + as.numeric(Galileo) + 
      as.numeric(BeiDou) + as.numeric(QZSS),
    geoid_model = NA_character_,
    base_model = rover_model,
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = "Mpala_ForestGEO",
    point_id = case_when(
      point_id == "quardrant 25" & Desc == "column60" ~ "quardrant 25_dup2",
      TRUE ~ point_id),
    corner = ifelse(
      point_id %in% c("quardrant 1", "quardrant 25", 
        "quardrant 25_01", "quardrant 01_column120"), 
      TRUE, FALSE),
    x_rel_m = case_when(
      point_id == "quardrant 1" ~ 0,
      point_id == "quardrant 13" ~ 0,
      point_id == "quardrant 12/13" ~ 2400,
      point_id == "quardrant 25" ~ 0,
      point_id == "quardrant 25_01" ~ 2400,
      point_id == "quardrant 01_column120" ~ 2400,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      point_id == "quardrant 1" ~ 0,
      point_id == "quardrant 13" ~ 250,
      point_id == "quardrant 12/13" ~ 250,
      point_id == "quardrant 25" ~ 500,
      point_id == "quardrant 25_01" ~ 500,
      point_id == "quardrant 01_column120" ~ 0,
      TRUE ~ NA_real_),
    crs_name = "UTM 37N",
    crs_epsg = 32637
  ) %>% 
  filter(!is.na(x_rel_m)) %>% 
  dplyr::select(all_of(pt_cols$column_name)) %>% 
  st_drop_geometry() 

# Clean stem data 
s_clean <- s %>% 
  filter(Census.Status %in% c("Finished", "New", "Forgotten Tree", "In Progress")) %>% 
  mutate(
    DBH = as.numeric(DBH),
    DBH.Old = as.numeric(DBH.Old),
    diam_cm = ifelse(DBH > 250 & DBH.Old < 200,
      DBH / 10, DBH),
    diam_cm = ifelse(diam_cm == 0, NA_real_, diam_cm),
    subplot_id = as.character(Q20)
  ) %>% 
  dplyr::select(
    Census.Status,
    subplot_id,
    x_rel_m = Px,
    y_rel_m = Py,
    x_grid_m = Qx,
    y_grid_m = Qy,
    x_utm_m = UTM.X..m.,
    y_utm_m = UTM.Y..m.,
    tree_id = TAG,
    stem_id = Stem.Tag,
    taxon_name = Specie,
    diam_cm,
    pom_m = HOM,
    measurement_date = Registered.Date,
    notes = Notes,
    Codes
  ) %>% 
  mutate(
    x_rel_m = as.numeric(x_rel_m),
    y_rel_m = as.numeric(y_rel_m),
    x_grid_m = as.numeric(x_grid_m),
    y_grid_m = as.numeric(y_grid_m),
    x_utm_m = as.numeric(x_utm_m),
    y_utm_m = as.numeric(y_utm_m),
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = "Mpala_ForestGEO",
    census_id = "3",
    census_date = as.character(mean(as.Date(measurement_date), na.rm = TRUE)),
    diam_cm = diam_cm / 10,
    growth_form = NA_character_,
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    alive = ifelse(grepl("D", Codes), "D", "A"),
    broken_above = ifelse(grepl("Q", Codes), "B", ""),
    broken_below = ifelse(grepl("X", Codes), "T", ""),
    broken_above = ifelse(broken_below == "T", "", broken_above),
    missing = ifelse(grepl("D2", Codes), "M", ""),
    fallen = ifelse(grepl("Y", Codes), "F", "S"),
    pom_m = ifelse(missing == "M", NA_real_, as.numeric(pom_m)),
    code = pasteVals(alive, broken_above, broken_below, missing, fallen)
  )

tree_vals <- s_clean %>% 
  dplyr::select(
    tree_id, 
    subplot_id,
    measurement_date, 
    census_date,
    taxon_name,
    x_rel_m,
    y_rel_m,
    x_grid_m,
    y_grid_m,
    x_utm_m,
    y_utm_m)

s_mult_clean <- s_mult %>% 
  mutate(
    DBH = case_when(
      DBH > 750 ~ DBH / 10,
      DBH > 200 & DBH.Old < 100 ~ DBH / 10,
      TRUE ~ DBH)) %>% 
  dplyr::select(
    Census.Status,
    tree_id = TAG,
    stem_id = Stem.Tag,
    pom_m = Hom,
    diam_cm = DBH,
    notes = Notes,
    Codes) %>% 
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = "Mpala_ForestGEO",
    census_id = "3",
    diam_cm = diam_cm / 10,
    growth_form = NA_character_,
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    alive = ifelse(grepl("D", Codes), "D", "A"),
    broken_above = ifelse(grepl("Q", Codes), "B", ""),
    broken_below = ifelse(grepl("X", Codes), "T", ""),
    broken_above = ifelse(broken_below == "T", "", broken_above),
    missing = ifelse(grepl("D2", Codes), "M", ""),
    pom_m = ifelse(missing == "M", NA_real_, pom_m),
    fallen = "S",
    code = pasteVals(alive, broken_above, broken_below, missing, fallen)
  ) %>% 
  left_join(., tree_vals, by = "tree_id")

# Clean taxonomic names
taxa$species_name <- paste(taxa$genus, taxa$species)
taxa$species_name <- gsub("NA NA", "Indet indet", taxa$species_name)
taxa$species_name <- gsub("Unidentified Unidentified", "Indet indet", taxa$species_name)

# Combine stem tables 
all(sort(names(s_clean)) == sort(names(s_mult_clean)))

# Prepare for transforming UTM coordinates to relative
origin_corner <- as.numeric(pt[
  pt$x_rel_m == 0 & pt$y_rel_m == 0, 
  c("rover_easting_utm_m", "rover_northing_utm_m")])

opp_corner <- as.numeric(pt[
  pt$x_rel_m == 2400 & pt$y_rel_m == 0, 
  c("rover_easting_utm_m", "rover_northing_utm_m")])

# Calculate the rotation angle (in radians)
# atan2 calculates the angle relative to the standard X-axis (East)
angle_rad <- atan2(
  opp_corner[2] - origin_corner[2], 
  opp_corner[1] - origin_corner[1])

s_out <- bind_rows(s_clean, s_mult_clean) %>% 
  filter(Census.Status %in% c("Finished", "New", "Forgotten Tree")) %>% 
  filter(!is.na(census_date)) %>% 
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(
    row = as.numeric(gsub(".{2}$", "", subplot_id)),
    column = as.numeric(sub(".*(.{2})$", "\\1", subplot_id)),
    x_rel_m = ifelse(is.na(x_rel_m), (column - 1) * 20 + x_grid_m, x_rel_m),
    y_rel_m = ifelse(is.na(y_rel_m), (row - 1) * 20 + y_grid_m, y_rel_m),
    dX = x_utm_m - origin_corner[1],
    dY = y_utm_m - origin_corner[2],
    x_rel_m = ifelse(is.na(x_rel_m),
      (dX * cos(angle_rad)) + (dY * sin(angle_rad)), 
      x_rel_m),
    y_rel_m = ifelse(is.na(y_rel_m),
      -(dX * sin(angle_rad)) + (dY * cos(angle_rad)),
      y_rel_m),
    height_m = NA_real_,
    record_id = row_number()) %>% 
  left_join(., taxa[,c("spcode", "species_name")], by = c("taxon_name" = "spcode")) %>% 
  dplyr::select(-taxon_name) %>% 
  rename(taxon_name = species_name) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Create plots table
plot_meta <- data.frame(
  site_id = param$site_id,
  acquisition_id = param$acquisition_id,
  plot_id = "Mpala_ForestGEO",
  census_id = unique(s_out$census_id),
  census_date = unique(s_out$census_date),
  plot_planar = TRUE,
  plot_length_m = 2400,
  plot_width_m = 500,
  meas_diam_min_cm = 2,
  meas_pom_default_m = 0.5,
  meas_tree_stem = TRUE,
  meas_tree_group = TRUE,
  meas_dead = FALSE,
  meas_fallen = FALSE,
  meas_liana = NA,
  meas_palm = NA,
  meas_bamboo = NA,
  meas_plot_loc = NA_character_,
  meas_stem_loc = "ForestGEO protocol. Well-surveyed 20x20 m subplot grid. Tape measures to locate stems by X and Y coordinates.",
  meas_protocol = NA_character_,
  notes_meas = NA_character_,
  forest_status = NA_character_,
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
# colCheck(pt, pt_cols)
colCheck(s_out, stem_cols)

# Check values
valCheck(
  plot = plot_meta, 
  stem = s_out, 
  pt = pt)

# Write corner points to file
write.csv(pt, file.path(outdir, "plot_pt.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(plot_meta, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_out, file.path(outdir, "stem.csv"), row.names = FALSE)

