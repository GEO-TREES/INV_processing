# Clean Robson Creek plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-13

# Packages
library(dplyr)
library(sf)
library(readxl)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Robson Creek"

# Define directories
indir <- "../../dat/sites/Robson Creek/raw"
outdir <- "../../dat/sites/Robson Creek/01_plot"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")

# TODO: Replace when plot coordinates received
# Import stem data
s <- read_excel(file.path(indir, "Robson_Creek_cleanedbiomass_predictedHeightsoutputASR2025septv4_Standardized2026.xlsx"),
  guess_max = Inf)

# Extract the SW corner of the 25 ha plot from the stem data
# Convert to UTM
sw_utm <- data.frame(longitude = min(s$longitude), latitude = min(s$latitude)) %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(., 32755) %>% 
  st_coordinates() %>% 
  c()

# Create the bounding box for the whole 25 ha area
bbox <- st_bbox(c(
  xmin = sw_utm[1], 
  ymin = sw_utm[2], 
  xmax = sw_utm[1] + 500, 
  ymax = sw_utm[2] + 500
), crs = 32755)

# Generate 1 ha grid cells
grid <- st_make_grid(bbox, cellsize = c(100, 100))

# Combine into a single data frame for easy viewing
all_corners <- lapply(seq_along(grid), function(i) {
  # Get coordinates for the i-th cell
  coords <- st_coordinates(grid[[i]])[1:4,]
  
  # Create a data frame for this specific 1 ha chunk
  data.frame(
    plot_id = as.character(i),
    corner_id = c("SW", "NW", "NE", "SE"),
    X = coords[, 1],
    Y = coords[, 2]
  )
})
names(all_corners) <- seq_along(all_corners)

# Convert each data frame into an sfc_POLYGON
poly_list <- lapply(all_corners, function(x) {
  # Extract only X and Y, convert to matrix
  coords_mat <- as.matrix(x[c(1:4,1), c("X", "Y")])
  
  # Create the polygon (requires a list of matrices)
  st_polygon(list(coords_mat))
})
names(poly_list) <- names(all_corners)

# Create final polygons object
poly <- st_sf(geometry = st_sfc(poly_list), crs = 32755) %>% 
  mutate(
    site_id = "Robson Creek",
    plot_id = as.character(names(poly_list)),
    area_reported_ha = 1,
    perim_reported_m = 400,
    .before = everything())

# Create final corner point object
pt <- do.call(rbind, all_corners) %>% 
  mutate(site_id = "Robson Creek", .before = everything()) %>% 
  mutate(
    x_rel_m = case_when(
      corner_id == "SW" ~ 0,
      corner_id == "SE" ~ 100,
      corner_id == "NW" ~ 0,
      corner_id == "NE" ~ 100,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      corner_id == "SW" ~ 0,
      corner_id == "SE" ~ 0,
      corner_id == "NW" ~ 100,
      corner_id == "NE" ~ 100,
      TRUE ~ NA_real_)) %>% 
  st_as_sf(., coords = c("X", "Y"), crs = 32755)

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

