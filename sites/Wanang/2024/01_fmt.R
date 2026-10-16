# Format Wanang tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-18

library(stringr)
library(ggplot2)

# Import data
pt <- read.csv(file.path(indir, "plot_corners.csv"))
poly <- st_read(file.path(indir, "Wanang_50ha_rotated.gpkg"))
plot_poly <- st_read(file.path(indir, "50haTopo_data_flies/Export_Outline.shp"))

# s <- read.csv(file.path(indir, "s.csv"))
s <- read_excel(file.path(indir, "Wanang_ForestGeo_3_censuses.xlsx"))

# censusID    census               scope
# 1           first                complete
# 2           Vincent PhD thesis   partial (52 quadrats)
# 3           ENSO study 2015      partial (178 quadrats)
# 4           second               complete
# 5           ENSO study 2017      partial (178 quadrats)
# 6           ENSO study 2019      partial (178 quadrats)
# NULL        third                incomplete data entry
# 
# So, delete records for PlotCensusNumber = 2,3,5 & 6, change 4->2 and
# NULL->3. Then things should make sense.
# 
# Not included in this package are the UTMs for the trees. I need to send
# you that separately.

s %>% 
  filter(
    PlotCensusNumber == 4,
    DBH >= 10) %>% 
  ggplot(., aes(x = PX, y = PY, colour = QX)) + 
  geom_point()

s %>% 
  filter(
    PlotCensusNumber == 4,
    DBH >= 10) %>% 
  ggplot(., aes(x = PX, y = PY, colour = QY)) + 
  geom_point()

s %>% 
  filter(
    PlotCensusNumber == 4,
    DBH >= 10) %>% 
  ggplot(., aes(x = QX, y = QY)) + 
  geom_point() + 
  coord_equal()


s_clean <- s %>% 
  mutate(
    census_id = case_when(
      PlotCensusNumber == 1 ~ "1",
      PlotCensusNumber == 4 ~ "2",
      is.na(PlotCensusNumber) ~ "3",
      TRUE ~ NA_character_)
    ) %>%
  filter(census_id == 2) %>%  # TODO:
  mutate(
    digits = str_split(trimws(QuadratName), "", simplify = TRUE),
    col = as.integer(str_sub(QuadratName, 1, 2)),
    row = as.integer(str_sub(QuadratName, 2 + 1, 2 * 2)),
    x_rel_m = QX + (20 * (col-1)),
    y_rel_m = QY + (20 * (row-1))
  ) %>% 
  rename(
    plot_id = PlotName,
    subplot_id = QuadratName,
    tree_id = Tag,
    stem_id = StemID,
    diam_cm = DBH,
    pom_m = HOM, 
    measurement_date = EaxctDate) %>%
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    subplot_id = as.character(subplot_id),
    tree_id = as.character(tree_id),
    stem_id = as.character(stem_id),
    record_id = row_number(),
    height_m = NA_real_,
    diam_cm = ifelse(diam_cm == 0, NA_real_, diam_cm),
    pom_m = ifelse(pom_m == 0, NA_real_, pom_m),
    pom_m = ifelse(is.na(diam_cm), NA_real_, pom_m),
    alive = ifelse(status %in% c("alive", "alivealive below"), "A", "D"),
    missing = ifelse(status %in% c("missing") & is.na(diam_cm), "M", ""),
    broken = "",
    fallen = "S", 
    code = pasteVals(alive, fallen, broken, missing),
    growth_form = NA_character_,
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    notes = NA_character_,
    taxon_name = paste(Genus, SpeciesName)) %>% 
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  group_by(plot_id) %>% 
  mutate(
    census_date = as.character(median(measurement_date, na.rm = TRUE)),
    measurement_date = as.character(measurement_date),
    measurement_date = ifelse(is.na(measurement_date), census_date, measurement_date)) %>% 
  ungroup() %>%
  dplyr::select(all_of(stem_cols$column_name))


ggplot() + 
  geom_point(data = s_clean, aes(x = x_rel_m, y = y_rel_m))


pt <- as.data.frame(st_coordinates(poly)[1:4, 1:2]) %>% 
  rename(
    "rover_easting_utm_m" = X,
    "rover_northing_utm_m" = Y) %>% 
  mutate(
    point_id = c("SW", "NW", "NE", "SE"),
    corner = TRUE,
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = "Wanang",
    crs_epsg = as.integer(32755),
    crs_name = "UTM 55S",
    x_rel_m = case_when(
      point_id == "SW" ~ 1000,
      point_id == "NW" ~ 1000,
      point_id == "NE" ~ 0,
      point_id == "SE" ~ 0,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      point_id == "SW" ~ 500,
      point_id == "NW" ~ 0,
      point_id == "NE" ~ 0,
      point_id == "SE" ~ 500,
      TRUE ~ NA_real_)) %>% 
  colGen(., pt_cols$column_name, pt_cols$class) %>% 
  dplyr::select(all_of(pt_cols$column_name))

ggplot()+ 
  geom_label(data = pt, aes(x = rover_easting_utm_m, y = rover_northing_utm_m, label = point_id))

# Create plot meta-data table
plots <- s_clean %>% 
  dplyr::select(site_id, acquisition_id, plot_id, census_date, census_id) %>% 
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
    meas_plot_loc = NA_character_, 
    meas_stem_loc = NA_character_, 
    vegetation_type = NA_character_, 
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
colCheck(pt, pt_cols)
colCheck(s_clean, stem_cols)

# Check values
valCheck(
  plot = plots,
  stem = s_clean,
  pt = pt)

# Write corner points to file
write.csv(pt, file.path(outdir, "plot_pt.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
    
