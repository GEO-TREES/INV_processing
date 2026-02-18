# Clean Bicuar tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-18

# Packages
library(dplyr)
library(tidyr)
library(sf)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Bicuar"

# Define directories
indir <- "../../dat/sites/Bicuar/raw"
outdir <- "../../dat/sites/Bicuar/01_fmt"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")
census_cols <- read.csv("../../templates/census_cols.csv")
stem_cols <- read.csv("../../templates/stem_cols.csv")

# Import data
s <- read.csv(file.path(indir, "stems.csv"))
p <- read.csv(file.path(indir, "plots.csv"))
plot_corners <- read_sf(file.path(indir, "plot_corners.shp"))

# Process plot corners
pt <- plot_corners %>% 
  mutate(
    site_id,
    corner_id = gsub(".*[0-9]+", "", name)) %>%
  dplyr::select(site_id, plot_id = plot_name, corner_id) %>% 
  st_transform(., crs = 32733) %>% 
  mutate(
    x_rel_m = case_when(
      corner_id %in% c("SW", "NW") ~ 0,
      corner_id %in% c("SE", "NE") ~ 100,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      corner_id %in% c("SW", "SE") ~ 0,
      corner_id %in% c("NW", "NE") ~ 100,
      TRUE ~ NA_real_)) %>% 
    dplyr::select(all_of(pt_cols$column_name))

# Create polygons
poly <- pt %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_convex_hull() %>% 
  ungroup()

# Create census meta-data table
census <- p %>% 
  dplyr::select(-plot_id) %>% 
  rename(
    plot_id = plot_name,
    min_diam_thresh_cm = min_diam_thresh) %>% 
  group_by(plot_id) %>% 
  mutate(census_id = dense_rank(census_date)) %>% 
  ungroup() %>% 
  mutate(
    site_id,
    min_diam_thresh_cm = as.numeric(min_diam_thresh_cm)) %>% 
  dplyr::select(all_of(census_cols$column_name))

# Prepare stem data 
s_clean <- s %>% 
  left_join(., unique(p[,c("plot_id", "plot_name")]), by = "plot_id") %>% 
  group_by(plot_name, stem_id) %>% 
  arrange(census_date) %>% 
  fill(x_grid, y_grid, .direction = "downup") %>% 
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
  group_by(plot_id) %>% 
  mutate(census_id = dense_rank(census_id)) %>% 
  ungroup() %>% 
  group_by(plot_id, census_id, stem_id) %>% 
  mutate(measurement_id = row_number()) %>% 
  ungroup() %>% 
  mutate(measurement_date = gsub("-01-01", "", measurement_date)) %>% 
  mutate(record_id = row_number()) %>% 
  dplyr::select(all_of(stem_cols$column_name))

# Check all columns in output objects
colCheck(poly, poly_cols)
colCheck(pt, pt_cols)
colCheck(census, census_cols)
colCheck(s_clean, stem_cols)

# Check values
polyValCheck(poly)
ptValCheck(pt)
censusValCheck(census)
stemValCheck(s_clean)

# Write polygons to file
st_write(poly, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write census meta-data to file
write.csv(census, file.path(outdir, "census.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
