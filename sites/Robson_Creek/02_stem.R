# Clean Robson Creek stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-13

# Packages
library(dplyr)
library(tidyr)
library(readxl)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Robson_Creek"

# Define directories
indir <- "../../dat/sites/Robson_Creek/raw"
outdir <- "../../dat/sites/Robson_Creek/02_stem"

# Import stem column descriptions
stem_cols <- read.csv("../../templates/stem_cols.csv")

# Import data
s <- read_excel(file.path(indir, "Robson_Creek_cleanedbiomass_predictedHeightsoutputASR2025septv4_Standardized2026.xlsx"),
  guess_max = Inf)

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
    measurement_date = phenomenonTime) %>% 
  mutate(
    site_id = site_id,
    plot_id = case_when(
      grepl("core1ha", plot_id) ~ "6",
      TRUE ~ gsub("Robson Creek, ha ", "", plot_id)),
    subplot_id = as.character(subplot_id),
    diam_cm = as.numeric(diam_cm),
    pom_m = as.numeric(pom_m),
    height_m = as.numeric(height_m),
    census_id = dense_rank(census_id),
    measurement_date = format(measurement_date),
    alive = case_when(
      alive == "Alive" ~ TRUE,
      alive == "Dead" ~ FALSE,
      is.na(alive) ~ TRUE,
      TRUE ~ NA),
    x_rel_m = as.numeric(x_rel_m),
    y_rel_m = as.numeric(y_rel_m),
    broken = ifelse(grepl("snapped", plantCondition, ignore.case = TRUE), TRUE, FALSE),
    fallen = FALSE,  # TODO:
    missing = FALSE,  # TODO:
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
  dplyr::select(all_of(stem_cols$column_name))

# Check all columns in output objects
colCheck(s_clean, stem_cols)

# Check values
stemValCheck(s_clean)

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)

