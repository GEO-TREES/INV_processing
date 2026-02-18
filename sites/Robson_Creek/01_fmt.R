# Clean Robson Creek plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-13

# Packages
library(dplyr)
library(tidyr)
library(sf)
library(readxl)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Robson_Creek"

# Define directories
indir <- "../../dat/sites/Robson_Creek/raw"
outdir <- "../../dat/sites/Robson_Creek/01_fmt"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")
stem_cols <- read.csv("../../templates/stem_cols.csv")
census_cols <- read.csv("../../templates/census_cols.csv")

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
    site_id,
    plot_id = as.character(names(poly_list))) %>% 
  dplyr::select(all_of(poly_cols$column_name))

# Create final corner point object
pt <- do.call(rbind, all_corners) %>% 
  mutate(site_id, .before = everything()) %>% 
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
  st_as_sf(., coords = c("X", "Y"), crs = 32755) %>% 
  dplyr::select(all_of(pt_cols$column_name))

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

# Prepare census table
census <- s_clean %>% 
  group_by(site_id, plot_id, census_id) %>% 
  summarise(census_date = format(mean(as.Date(measurement_date), na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(min_diam_thresh_cm = 10) %>% 
  dplyr::select(all_of(census_cols$column_name))

# Check all columns in output objects
colCheck(poly, poly_cols)
colCheck(pt, pt_cols)
colCheck(s_clean, stem_cols)
colCheck(census, census_cols)

# Check values
polyValCheck(poly)
ptValCheck(pt)
stemValCheck(s_clean)
censusValCheck(census)

# Write polygons to file
st_write(poly, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write corner points to file
st_write(pt, file.path(outdir, "plot_pt.gpkg"), delete_dsn = TRUE)

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)

# Write census table to file
write.csv(census, file.path(outdir, "census.csv"), row.names = FALSE)
