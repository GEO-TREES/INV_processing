# Clean Bicuar stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(tidyr)

# Define site ID
site_id <- "Bicuar"

# Define directories
indir <- "../../dat/sites/Bicuar/raw"
outdir <- "../../dat/sites/Bicuar/02_stem"

# Import stem column descriptions
stem_cols <- read.csv("../../dat/templates/stem_cols.csv")

# Import data
s <- read.csv(file.path(indir, "stems.csv"))
p <- read.csv(file.path(indir, "plots.csv"))

# Prepare stem data 
s_clean <- s %>% 
  group_by(plot_id, stem_id) %>% 
  arrange(census_date) %>% 
  fill(x_grid, y_grid, .direction = "down") %>% 
  ungroup() %>% 
  filter(grepl("2024", census_date)) %>% 
  left_join(., unique(p[,c("plot_id", "plot_name")]), by = "plot_id") %>% 
  dplyr::select(-plot_id) %>% 
  rename(plot_id = plot_name) %>% 
  mutate(
    site_id,
    census_id = gsub("-.*", "", census_date)) %>% 
  group_by(site_id, plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(
    measurement_id = paste(site_id, plot_id, census_id, stem_id, measurement_id, sep = ":"),
    stem_id = paste(site_id, plot_id, census_id, stem_id, sep = ":"),
    census_id = paste(site_id, plot_id, census_id, sep = ":"),
    plot_id = paste(site_id, plot_id, sep = ":")
  ) %>% 
  mutate(
    alive = ifelse(stem_status %in% c("a", "r"), 1, 0),
    broken = ifelse(grepl("b|p", stem_mode), 1, 0),
    fallen = ifelse(grepl("f", stem_mode), 1, 0),
    missing = ifelse(grepl("v|q", stem_mode), 1, 0),
    liana = ifelse(grepl("w", stem_mode), 1, 0)) %>% 
  dplyr::select(
    site_id,
    plot_id,
    census_id,
    stem_id,
    measurement_id,
    census_date,
    x_rel = x_grid,
    y_rel = y_grid,
    diam,
    pom,
    height, 
    taxon_name_orig = species_name_clean,
    alive,
    broken,
    fallen,
    missing,
    liana)

# Check all columns in stems table
stopifnot(all(colnames(s_clean) == stem_cols$column_name))

# Write data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
