# Clean Bicuar plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(sf)

# Define site ID
site_id <- "Bicuar"

# Source functions
source("../../func.R")

# Define directories
indir <- "../../dat/sites/Bicuar/raw"
outdir <- "../../dat/sites/Bicuar/01_polys"

# Import column descriptions
polys_cols <- read.csv("../../dat/templates/polys_cols.csv")
pts_cols <- read.csv("../../dat/templates/pts_cols.csv")

# Import plot corners
pts <- read_sf(file.path(indir, "plot_corners.shp")) %>% 
  mutate(
    site_id,
    plot_id = paste(site_id, plot_name, sep = ":"),
    corner_id = gsub(".*[0-9]+", "", name)) %>%
  dplyr::select(site_id, plot_id, corner_id) %>% 
  st_transform(., crs = 32733) %>% 
  mutate(
    x_rel = case_when(
      corner_id %in% c("SW", "NW") ~ 0,
      corner_id %in% c("SE", "NE") ~ 100,
      TRUE ~ NA_real_),
    y_rel = case_when(
      corner_id %in% c("SW", "SE") ~ 0,
      corner_id %in% c("NW", "NE") ~ 100,
      TRUE ~ NA_real_)) %>% 
  relocate(geometry, .after = last_col())

# Create polygons
polys <- pts %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_convex_hull() %>% 
  mutate(
    area_reported_ha = 1,
    perim_reported_m = 400) %>% 
  relocate(geometry, .after = last_col())

# Check all columns in output objects
stopifnot(all(colnames(polys) == polys_cols$column_name))
stopifnot(all(colnames(pts) == pts_cols$column_name))

# Write polygons to file
st_write(polys, file.path(outdir, "polys.gpkg"), delete_dsn = TRUE)

# Write origin points to file
st_write(pts, file.path(outdir, "pts.gpkg"), delete_dsn = TRUE)

