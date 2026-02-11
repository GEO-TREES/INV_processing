# Clean Bicuar plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(sf)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Bicuar"

# Define directories
indir <- "../../dat/sites/Bicuar/raw"
outdir <- "../../dat/sites/Bicuar/01_plot"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")

# Import plot corners
pt <- read_sf(file.path(indir, "plot_corners.shp")) %>% 
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
  relocate(geometry, .after = last_col())

# Create polygons
poly <- pt %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_convex_hull() %>% 
  ungroup() %>% 
  mutate(
    area_reported_ha = 1,
    perim_reported_m = 400) %>% 
  relocate(geometry, .after = last_col())

# Check all columns in output objects
colCheck(poly, poly_cols)
colCheck(pt, pt_cols)

# Check values
polyValCheck(poly)
ptValCheck(pt)

# Write polygons to file
st_write(poly, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)
