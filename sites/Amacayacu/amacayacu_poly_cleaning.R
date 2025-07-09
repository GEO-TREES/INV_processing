# Clean Amacayacu plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-08

# Packages
library(dplyr)
# library(ggplot2)
library(sf)

# Define site ID
BRM_site <- "Amacayacu"

# Source functions
source("../../func.R")

# Define directories
indir <- file.path("../../../dat/raw", BRM_site)
outdir <- file.path("../../../dat/clean", BRM_site)

# Read in plot polygons as sf objects
# Gigante plot
crn <- read.csv(file.path(indir, "plot_corners.csv"))

# Create sf object from corners
pts <- st_as_sf(crn, coords = c("longitude", "latitude"), crs = 4326) %>% 
  rename(corner_id = corner) %>% 
  mutate(
    site_id = BRM_site,
    plot_id = "Amacayacu", 
    .before = everything())

# Create polygon from points
polys <- pts %>% 
  summarise() %>% 
  st_convex_hull() %>% 
  mutate(
    site_id = BRM_site,
    plot_id = "Amacayacu", 
    .before = everything())

# Write polygons to file
st_write(polys, file.path(outdir, "polys.gpkg"), delete_dsn = TRUE)

# Write origin points to file
st_write(pts, file.path(outdir, "pts.gpkg"), delete_dsn = TRUE)

