# Format SEOSAW data for GEO-TREES
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-09-07

# This script provides a generic template for converting tree inventory data
# from the SEOSAW database into the format required for the GEO-TREES tree
# inventory data processing pipeline. This script should be amended depending
# on the particular dataset.

# Packages
library(dplyr)
library(tidyr)
library(yaml)

# Source functions
source("../func.R")

# Define input directory containing data from a single site
indir <- "~/git_proj/seosaw_data/data_clean/ABG"

# Define output directory
outdir <- "./"

# Import param.yaml
param <- read_yaml("../sites/Bicuar/2024-02-12/v1/param.yaml")

# Import data
s <- read.csv(file.path(indir, "stems.csv"))
p <- read.csv(file.path(indir, "plots.csv"))

# Import column definitions
plot_cols <- read.csv("../templates/plot_cols.csv")
stem_cols <- read.csv("../templates/stem_cols.csv")

# Prepare stem data 
s_clean <- s %>% 
  group_by(plot_id, stem_id) %>% 
  arrange(census_date) %>% 
  fill(x_grid, y_grid, subplot_id, .direction = "downup") %>% 
  ungroup() %>% 
  rename(
    measurement_date = census_date,
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
    fallen = ifelse(grepl("f", stem_mode), "F", "S"),
    broken = ifelse(grepl("b|p", stem_mode), "B", ""),
    missing = ifelse(grepl("v|q", stem_mode), "M", ""),
    stump = ifelse(grepl("t", stem_mode), "T", ""),
    hollow = ifelse(grepl("g", stem_mode), "H", ""),
    buttress = "",  # TODO
    code = pasteVals(alive, fallen, missing, broken, stump, hollow, buttress), 
    growth_form = NA_character_,
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    subplot_id = as.character(subplot_id)) %>% 
  group_by(plot_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Create plots table
p_clean <- p %>% 
  group_by(plot_id) %>% 
  filter(census_date == max(census_date)) %>% 
  rename(
    plot_width_m = plot_width,
    plot_length_m = plot_length,
    plot_slope_deg = slope,
    plot_aspect_deg = aspect,
    plot_elevation_m = elevation,
    notes_plot = plot_notes,
    meas_diam_min_cm = min_diam_thresh,
    meas_pom_default_m = pom_default,
    meas_tree_group = tree_diff,
    meas_dead = dead_stems_sampled,
    meas_liana = lianas_sampled,
    meas_stem_loc = xy_method,
    ) %>%
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_length_m = as.numeric(plot_length_m),
    plot_width_m = as.numeric(plot_width_m),
    meas_diam_min_cm = as.numeric(meas_diam_min_cm),
    meas_tree_stem = ifelse(meas_tree_stem == "stem", TRUE, FALSE), 
    plot_planar = FALSE,
    meas_fallen = TRUE,  # TODO
    meas_palm = NA,  # TODO
    meas_bamboo = NA,  # TODO
    meas_plot_loc = NA_character_,  # TODO
    meas_protocol = "SEOSAW_v3.6",  # TODO
    notes_meas = NA_character_,  # TODO
    forest_status = NA_character_,  # TODO
    vegetation_type = NA_character_,  # TODO
    land_use = NA_character_,  # TODO
    treatment = NA_character_,  # TODO
    treatment_ref = NA_character_,  # TODO
    fire_regime = NA_character_,  # TODO
    cyclone_regime = NA_character_,  # TODO
    flood_regime = NA_character_,  # TODO
    earth_regime = NA_character_,  # TODO
    herbivory_regime = NA_character_,  # TODO
    notes_disturbance = NA_character_  # TODO
    ) %>% 
  dplyr::select(all_of(plot_cols$column_name))

# Check all columns in output objects
colCheck(p_clean, plot_cols)
colCheck(pt_clean, pt_cols)
colCheck(s_clean, stem_cols)

# Check values
valCheck(
  plot = p_clean, 
  stem = s_clean, 
  pt = pt_clean
  )

# Write corner points to file
write.csv(pt_clean, file.path(outdir, "plot_pt.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(p_clean, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)

