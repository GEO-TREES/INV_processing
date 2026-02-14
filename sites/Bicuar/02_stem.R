# Clean Bicuar stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(tidyr)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Bicuar"

# Define directories
indir <- "../../dat/sites/Bicuar/raw"
outdir <- "../../dat/sites/Bicuar/02_stem"

# Import stem column descriptions
stem_cols <- read.csv("../../templates/stem_cols.csv")

# Import data
s <- read.csv(file.path(indir, "stems.csv"))
p <- read.csv(file.path(indir, "plots.csv"))

# Prepare stem data 
s_clean <- s %>% 
  left_join(., unique(p[,c("plot_id", "plot_name")]), by = "plot_id") %>% 
  group_by(plot_name, stem_id) %>% 
  arrange(census_date) %>% 
  fill(x_grid, y_grid, .direction = "down") %>% 
  ungroup() %>% 
  dplyr::select(-plot_id) %>% 
  rename(
    measurement_date = census_date,
    plot_id = plot_name,
    x_rel_m = x_grid,
    y_rel_m = y_grid,
    diam_cm = diam,
    pom_m = pom,
    height_m = height, 
    taxon_name = species_name_clean) %>% 
  mutate(
    census_id = as.numeric(gsub("-.*", "", measurement_date)),
    site_id = site_id,
    alive = ifelse(stem_status %in% c("a", "r"), TRUE, FALSE),
    broken = ifelse(grepl("b|p", stem_mode), TRUE, FALSE),
    fallen = ifelse(grepl("f", stem_mode), TRUE, FALSE),
    missing = ifelse(grepl("v|q", stem_mode), TRUE, FALSE),
    agb_allometry = NA_character_,
    subplot_id = as.character(subplot_id)) %>% 
  group_by(plot_id, census_id) %>% 
  mutate(census_date = as.character(mean(as.Date(measurement_date), na.rm = TRUE))) %>% 
  ungroup() %>% 
  group_by(plot_id) %>% 
  mutate(census_id = dense_rank(census_id)) %>% 
  ungroup() %>% 
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(
    census_date = gsub("-01-01", "", census_date),
    measurement_date = gsub("-01-01", "", measurement_date)) %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Check all columns in output objects
colCheck(s_clean, stem_cols)

# Check values
stemValCheck(s_clean)

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
