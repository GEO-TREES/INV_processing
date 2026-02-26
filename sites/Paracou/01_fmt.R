# Clean Paracou plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-11

# Packages
library(dplyr)
library(tidyr)
library(sf)

# Source functions
# source("./func.R")

# Define site ID
# site_id <- "Paracou"

# Define directories
# indir <- "./dat/sites/Paracou/raw"
# outdir <- "./dat/sites/Paracou/01_fmt"

# Import column descriptions
# plot_cols <- read.csv("./templates/plot_cols.csv")
# pt_cols <- read.csv("./templates/pt_cols.csv")
# census_cols <- read.csv("./templates/census_cols.csv")
# stem_cols <- read.csv("./templates/stem_cols.csv")

# Import stem data
s_P13 <- read.csv(file.path(indir, "Paracou Biodiversity Plots/2024-08-29_ParacouP13AllYears.csv"))
s_P14 <- read.csv(file.path(indir, "Paracou Biodiversity Plots/2024-08-29_ParacouP14AllYears.csv"))
s_P15 <- read.csv(file.path(indir, "Paracou Biodiversity Plots/2024-08-29_ParacouP15AllYears.csv"))
s_P16 <- read.csv(file.path(indir, "Paracou Biodiversity Plots/2023-09-29_ParacouP16AllYears.csv"))

s_P2 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level1 Treatment Plots/2024-04-18_ParacouP2AllYears.csv"))
s_P7 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level1 Treatment Plots/2024-04-18_ParacouP7AllYears.csv"))
s_P9 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level1 Treatment Plots/2024-04-18_ParacouP9AllYears.csv"))

s_P3 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level2 Treatment Plots/2024-04-18_ParacouP3AllYears.csv"))
s_P5 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level2 Treatment Plots/2024-04-18_ParacouP5AllYears.csv"))
s_P10 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level2 Treatment Plots/2024-08-08_ParacouP10AllYears.csv"))

s_P4 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level3 Treatment Plots/2024-04-18_ParacouP4AllYears.csv"))
s_P8 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level3 Treatment Plots/2024-04-18_ParacouP8AllYears.csv"))
s_P12 <- read.csv(file.path(indir, "Paracou Disturbance Experiment - Level3 Treatment Plots/2024-04-18_ParacouP12AllYears.csv"))

# Check all columns are identical 
stopifnot(all(
  names(s_P13) == names(s_P14),
  names(s_P13) == names(s_P15),
  names(s_P13) == names(s_P16),
  names(s_P13) == names(s_P2),
  names(s_P13) == names(s_P7),
  names(s_P13) == names(s_P9),
  names(s_P13) == names(s_P3),
  names(s_P13) == names(s_P5),
  names(s_P13) == names(s_P10),
  names(s_P13) == names(s_P4),
  names(s_P13) == names(s_P8),
  names(s_P13) == names(s_P12)
))

s <- bind_rows(
  s_P13, 
  s_P14,
  s_P15,
  s_P16,
  s_P2,
  s_P7,
  s_P9,
  s_P3,
  s_P5,
  s_P10,
  s_P4,
  s_P8,
  s_P12)

# Import plot metadata
plot_meta <- read.csv(file.path(indir, "ParacouDescription.csv"))

# p16_subplot_layout <- plot_meta %>% 
#   filter(Plot == "16") %>% 
#   pivot_longer(
#     cols = starts_with("SubPlotL"),
#     names_to = c(".value", "corner_id"),
#     names_pattern = "SubPlot(Lat|Lon)(SW|SE|NE|NW)") %>% 
#   dplyr::select(
#     subplot_id = SubPlot,
#     corner_id,
#     longitude = Lon,
#     latitude = Lat) %>% 
#   st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
#   st_transform(., crs = 32622) %>%  # UTM 22N
#   group_by(subplot_id) %>% 
#   summarise() %>% 
#   st_centroid() 
# ggplot() + geom_sf_label(data = test, aes(label = subplot_id))

# Extract plot corners
pt <- plot_meta %>% 
  pivot_longer(
    cols = starts_with("PlotL"),
    names_to = c(".value", "corner_id"),
    names_pattern = "Plot(Lat|Lon)(SW|SE|NE|NW)") %>% 
  dplyr::select(
    plot_id = Plot,
    corner_id,
    longitude = Lon,
    latitude = Lat, 
    PlotArea) %>% 
  filter(plot_id != "17(Arbocel)") %>% 
  distinct() %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  mutate(
    site_id,
    x_rel_m = case_when(
      corner_id %in% c("SW", "NW") ~ 0,
      corner_id %in% c("SE", "NE") & PlotArea == 6.25 ~ 250,
      corner_id %in% c("SE", "NE") & PlotArea == 25 ~ 500,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      corner_id %in% c("SW", "SE") ~ 0,
      corner_id %in% c("NW", "NE") & PlotArea == 6.25 ~ 250,
      corner_id %in% c("NW", "NE") & PlotArea == 25 ~ 500,
      TRUE ~ NA_real_)) %>% 
  filter(!plot_id %in% c("1", "11", "6")) %>% 
  dplyr::select(all_of(pt_cols$column_name))

# Prepare stem data 
s_clean <- s %>% 
  rename(
    plot_id = Plot,
    subplot_id = SubPlot,
    tree_id = idTree,
    x_rel_m = Xfield,
    y_rel_m = Yfield,
    census_id = CensusYear,
    measurement_date = CensusDate,
    alive = CodeAlive) %>% 
  mutate(
    site_id,
    plot_id = as.character(plot_id),
    subplot_id = as.character(subplot_id),
    tree_id = as.character(tree_id),
    stem_id = NA_character_,
    pom_m = POM * 0.01,
    diam_cm = ifelse(is.na(CircCorr), Circ / pi, CircCorr / pi),
    height_m = NA_real_,
    alive = as.logical(alive),
    taxon_name = paste(trimws(GenusFilled), trimws(SpeciesFilled)),
    broken = FALSE,
    fallen = ifelse(MeasCode == 12 , TRUE, FALSE),
    missing = FALSE,
    agb_allometry = NA_character_) %>% 
  group_by(plot_id) %>% 
  mutate(census_id = dense_rank(census_id)) %>% 
  ungroup() %>% 
  group_by(plot_id, tree_id, stem_id, census_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  group_by(plot_id, census_id) %>% 
  mutate(census_date = format(mean(as.Date(measurement_date)))) %>% 
  ungroup() %>% 
  filter(as.Date(census_date) > as.Date("2017-01-01")) %>% 
  mutate(record_id = row_number()) %>% 
  mutate(
    col = (as.numeric(subplot_id) - 1) %% 5,
    row = 4 - ((as.numeric(subplot_id) - 1) %/% 5),
    x_rel_m = case_when(
      plot_id == "16" ~ x_rel_m + 100 * col,
      TRUE ~ x_rel_m),
    y_rel_m = case_when(
      plot_id == "16" ~ y_rel_m + 100 * row,
      TRUE ~ y_rel_m)) %>% 
  dplyr::select(all_of(stem_cols$column_name))

census <- s_clean %>% 
  group_by(plot_id, census_id) %>% 
  summarise(census_date = format(mean(as.Date(measurement_date)))) %>% 
  ungroup() %>% 
  mutate(
    site_id,
    min_diam_thresh_cm = 10) %>% 
  dplyr::select(all_of(census_cols$column_name))

# Create plots table
plots <- census %>% 
  group_by(site_id, plot_id) %>% 
  summarise(
    census_date_all = paste(census_date, collapse = ";"),
    census_date_geotrees = max(census_date)) %>% 
  ungroup() %>% 
  mutate(
    plot_width_m = case_when(
      plot_id == 16 ~ 500, 
      TRUE ~ 250),
    plot_length_m = case_when(
      plot_id == 16 ~ 500, 
      TRUE ~ 250),
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
    
# Check all columns in output objects
colCheck(plots, plot_cols)
colCheck(pt, pt_cols)
colCheck(s_clean, stem_cols)
colCheck(census, census_cols)

# Check values
# plotValCheck(plots)
ptValCheck(pt)
stemValCheck(s_clean)
censusValCheck(census)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)

# Write stem data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write census table to file
write.csv(census, file.path(outdir, "census.csv"), row.names = FALSE)

