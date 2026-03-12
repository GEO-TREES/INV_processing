# Clean Robson Creek plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-13

# TODO: Replace when plot coordinates received
# Import stem data
s <- read_excel(file.path(indir, "Robson_Creek_cleanedbiomass_predictedHeightsoutputASR2025septv4_Standardized2026.xlsx"),
  guess_max = Inf)

# Extract the SW corner of the 25 ha plot from the stem data
# Convert to UTM
sw_utm <- data.frame(longitude = min(s$longitude), latitude = min(s$latitude)) %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(., 32755) %>% 
  st_coordinates() %>% 
  c()

# Create the bounding box for the whole 25 ha area
bbox <- st_bbox(c(
  xmin = sw_utm[1], 
  ymin = sw_utm[2], 
  xmax = sw_utm[1] + 500, 
  ymax = sw_utm[2] + 500
), crs = 32755)

# Generate 1 ha grid cells
grid <- st_make_grid(bbox, cellsize = c(100, 100))

# Combine into a single data frame for easy viewing
all_corners <- lapply(seq_along(grid), function(i) {
  # Get coordinates for the i-th cell
  coords <- st_coordinates(grid[[i]])[1:4,]
  
  # Create a data frame for this specific 1 ha chunk
  data.frame(
    plot_id = as.character(i),
    point_id = c("SW", "NW", "NE", "SE"),
    X = coords[, 1],
    Y = coords[, 2]
  )
})
names(all_corners) <- seq_along(all_corners)

# Prepare stem data 
s_clean <- s %>% 
  rename(
    plot_id = plotID,
    subplot_id = subplotID,
    x_rel_m = positionX_Coordinate,
    y_rel_m = positionY_Coordinate,
    taxon_name = scientificName,
    tree_id = plantID,
    stem_id = stemID,
    diam_cm = stemDiameter_centimetres,
    pom_m = stemDiameterPointOfMeasurement_metres,
    height_m = stemHeight_metres,
    alive = plantMortality,
    census_id = year,
    measurement_date = phenomenonTime,
    notes = comment) %>% 
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = case_when(
      grepl("core1ha", plot_id) ~ "6",
      TRUE ~ gsub("Robson Creek, ha ", "", plot_id)),
    subplot_id = as.character(subplot_id),
    diam_cm = as.numeric(diam_cm),
    pom_m = as.numeric(pom_m),
    height_m = as.numeric(height_m),
    census_id = dense_rank(census_id),
    measurement_date = format(measurement_date),
    notes = ifelse(notes == "NA", NA_character_, notes),
    alive = case_when(
      alive == "Alive" ~ "A",
      alive == "Dead" ~ "D",
      is.na(alive) ~ "A",
      TRUE ~ NA),
    x_rel_m = as.numeric(x_rel_m),
    y_rel_m = as.numeric(y_rel_m),
    broken = ifelse(grepl("snapped", plantCondition, ignore.case = TRUE), "B", ""),
    fallen = "S",  # TODO:
    missing = "",  # TODO:
    stump = "",  # TODO:
    code = pasteVals(alive, broken, fallen, missing, stump),
    agb_allometry = NA_character_,
    subplot_in_plot = (as.numeric(subplot_id) - 1) %% 25,
    col = subplot_in_plot %% 5,
    row = subplot_in_plot %/% 5,
    x_rel_m = col * 20 + x_rel_m,
    y_rel_m = row * 20 + y_rel_m) %>% 
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  group_by(plot_id, census_id) %>% 
  mutate(census_date = format(mean(as.Date(measurement_date), na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(record_id = row_number()) %>% 
  filter(as.Date(census_date) > as.Date("2023-01-01")) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Prepare plots table
plots <- s_clean %>% 
  dplyr::select(site_id, acquisition_id, plot_id, census_date) %>% 
  distinct() %>% 
  mutate(
    min_diam_thresh_cm = 10,
    plot_width_m = 100,
    plot_length_m = 100,
    plot_slope_deg = NA_real_,
    plot_aspect_deg = NA_real_,
    plot_elevation_m = NA_real_,
    plot_planar = TRUE,
    notes_plot = NA_character_,
    meas_diam_min_cm = 10,
    meas_pom_default_m = 1.3,
    meas_tree_stem = TRUE,
    meas_tree_group = TRUE,
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

# Create final corner point object
pt <- do.call(rbind, all_corners) %>% 
  filter(plot_id %in% plots$plot_id) %>% 
  mutate(
    site_id = param$site_id, 
    acquisition_id = param$acquisition_id, 
    .before = everything()) %>% 
  mutate(
    x_rel_m = case_when(
      point_id == "SW" ~ 0,
      point_id == "SE" ~ 100,
      point_id == "NW" ~ 0,
      point_id == "NE" ~ 100,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      point_id == "SW" ~ 0,
      point_id == "SE" ~ 0,
      point_id == "NW" ~ 100,
      point_id == "NE" ~ 100,
      TRUE ~ NA_real_)) %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 32755) %>% 
  st_transform(., 4326) %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Check all columns in output objects
colCheck(plots, plot_cols)
colCheck(pt, pt_cols)
colCheck(s_clean, stem_cols)

# Check values
valCheck(
  plot = plots,
  stem = s_clean,
  pt = pt)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

