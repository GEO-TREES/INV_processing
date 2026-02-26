# Clean Danum Valley tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-09-30

# Packages
library(dplyr)
library(tidyr)
library(sf)

# Source functions
# source("./func.R")

# Define site ID
# site_id <- "DanumValley"

# Define directories
# indir <- "./dat/sites/DanumValley/raw"
# outdir <- "./dat/sites/DanumValley/01_fmt"

# Import column descriptions
# stem_cols <- read.csv("./templates/stem_cols.csv")
# census_cols <- read.csv("./templates/census_cols.csv")
# plot_cols <- read.csv("./templates/plot_cols.csv")
# pt_cols <- read.csv("./templates/pt_cols.csv")

# Import data
s <- read.table(file.path(indir, "ViewFullTable_danum.txt"),
  sep = "\t", header = TRUE, colClasses = "character")
plot_corners <- read.csv(file.path(indir, "corner_coords.csv"))

# Process plot corners
pt <- plot_corners %>% 
  mutate(
    site_id,
    plot_id = unique(s$PlotName),
    x_rel_m = case_when(
      name == "SE_0000" ~ 0,
      name == "SW_0025" ~ 0,
      name == "NE_5000" ~ 1000,
      name == "NW_5025" ~ 1000),
    y_rel_m = case_when(
      name == "SE_0000" ~ 0,
      name == "SW_0025" ~ 500,
      name == "NE_5000" ~ 0,
      name == "NW_5025" ~ 500),
    corner_id = gsub("_.*", "", name)) %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(all_of(pt_cols$column_name))

# Clean stem data
s_sel <- s %>% 
  rename(
    plot_id = PlotName,
    subplot_id = QuadratID,
    x_rel_m = PX,
    y_rel_m = PY,
    tree_id = TreeID,
    stem_id = StemID,
    census_id = CensusID,
    diam_cm = DBH,
    pom_m = HOM,
    measurement_date = ExactDate,
    alive = Status,
    flags = ListOfTSM) %>% 
  mutate(
    site_id,
    taxon_name = paste(Genus, SpeciesName),
    x_rel_m = as.numeric(x_rel_m),
    y_rel_m = as.numeric(y_rel_m),
    census_id = as.numeric(census_id),
    diam_cm = as.numeric(diam_cm) * 0.1,
    pom_m = as.numeric(pom_m) * 0.01,
    census_id = as.integer(census_id),
    height_m = NA_real_,
    agb_allometry = NA_character_,
    measurement_date = as.character(measurement_date),
    measurement_date = ifelse(measurement_date == "NULL", NA_character_, measurement_date),
    flags = ifelse(flags == "NULL", NA_character_, flags),
    broken = ifelse(grepl("X|Q", flags), TRUE, FALSE),
    fallen = ifelse(grepl("Y", flags), TRUE, FALSE),
    missing = ifelse(alive == "missing", TRUE, FALSE),
    alive = case_when(
      alive == "alive" ~ TRUE,
      alive == "dead" ~ FALSE,
      alive == "missing" ~ FALSE,
      alive == "broken below" ~ TRUE,
      TRUE ~ NA)) %>% 
  group_by(census_id) %>% 
  mutate(
    measurement_date = ifelse(is.na(measurement_date), 
      as.character(median(as.Date(measurement_date), na.rm = TRUE)), 
      measurement_date)) %>% 
  ungroup() %>% 
  filter(
    census_id == 2,
    !stem_id %in% c("216631", "88078", "249295")) %>% 
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Create census table
census <- s_sel %>% 
  group_by(site_id, plot_id, census_id) %>% 
  summarise(
    census_date = as.character(median(as.Date(measurement_date), na.rm = TRUE)),
    measurement_date_min = as.character(min(as.Date(measurement_date), na.rm = TRUE)),
    measurement_date_max = as.character(max(as.Date(measurement_date), na.rm = TRUE))) %>% 
  ungroup() %>% 
  dplyr::select(all_of(census_cols$column_name))

# Create plot table
census_date_all <- s %>% 
  group_by(CensusID) %>% 
  summarise(census_date = as.character(median(as.Date(ExactDate), na.rm = TRUE))) %>% 
  ungroup() %>% 
  pull(census_date) %>% 
  paste(., collapse = ";")

plots <- data.frame(
    site_id,
    plot_id = unique(census$plot_id),
    census_date_geotrees = census$census_date,
    census_date_all,
    plot_width_m = 500,
    plot_length_m = 1000,
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
    meas_liana = FALSE,
    meas_palm = NA,
    meas_bamboo = NA,
    meas_protocol = NA_character_,
    notes_meas = NA_character_,
    forest_status = "Old growth",
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
colCheck(census, census_cols)
colCheck(s_sel, stem_cols)

# Check values
# plotValCheck(plots)
ptValCheck(pt)
censusValCheck(census)
stemValCheck(s_sel)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write census meta-data to file
write.csv(census, file.path(outdir, "census.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(plots, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem measurements table to file
write.csv(s_sel, file.path(outdir, "stem.csv"), row.names = FALSE)

