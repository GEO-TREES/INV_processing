# Clean Misiones plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(sf)

# Define site ID
site_id <- "Misiones"

# Source functions
source("../../func.R")

# Define directories
indir <- "../../dat/sites/Misiones/raw"
outdir <- "../../dat/sites/Misiones/01_polys"

# Import plot corners
unzip(file.path(indir, "parcela_ubicacion_campo.kmz"), exdir = indir)
polys <- st_read(file.path(indir, "doc.kml")) %>% 
  mutate(
    area_reported_ha = 1,
    perim_reported_m = 400,
    site_id = site_id,
    plot_name = as.character(row_number())) %>% 
  dplyr::select(site_id, plot_name, area_reported_ha, perim_reported_m, geometry) %>% 
  st_transform(., 32721)

#site_cent <- st_coordinates(st_centroid(summarise(polys)))
#latLong2UTM(site_cent[1], site_cent[2])

# Extract plot corners
# Assign metadata to points
pts <- polyCornerExtract(polys, 
  corner = rep(list(c("SW", "NW", "NE", "SE")), nrow(polys)), 
  name = c("site_id", "plot_name")) %>%
  mutate(site_id = site_id) %>% 
  dplyr::select(site_id, plot_name, corner_id) %>% 
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

# Write polygons to file
st_write(polys, file.path(outdir, "polys.gpkg"), delete_dsn = TRUE)

# Write origin points to file
st_write(pts, file.path(outdir, "pts.gpkg"), delete_dsn = TRUE)

