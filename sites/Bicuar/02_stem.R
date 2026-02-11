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
  filter(grepl("2024", census_date)) %>% 
  dplyr::select(-plot_id) %>% 
  rename(
    plot_id = plot_name,
    x_rel_m = x_grid,
    y_rel_m = y_grid,
    diam_cm = diam,
    pom_m = pom,
    height_m = height, 
    taxon_name = species_name_clean) %>% 
  mutate(
    site_id = site_id,
    census_id = as.integer(3),
    alive = ifelse(stem_status %in% c("a", "r"), TRUE, FALSE),
    broken = ifelse(grepl("b|p", stem_mode), TRUE, FALSE),
    fallen = ifelse(grepl("f", stem_mode), TRUE, FALSE),
    missing = ifelse(grepl("v|q", stem_mode), TRUE, FALSE),
    liana = ifelse(grepl("w", stem_mode), TRUE, FALSE), 
    agb_allometry = NA_character_,
    subplot_id = as.character(subplot_id)) %>% 
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Check all columns in output objects
colCheck(s_clean, stem_cols)

# Check values
stemValCheck(s_clean)

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
