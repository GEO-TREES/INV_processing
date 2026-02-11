# Clean Paracou plot polygons data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-11

# Packages
library(dplyr)
library(tidyr)
library(sf)

# Source functions
source("../../func.R")

# Define site ID
site_id <- "Paracou"

# Define directories
indir <- "../../dat/sites/Paracou/raw"
outdir <- "../../dat/sites/Paracou/01_plot"

# Import column descriptions
poly_cols <- read.csv("../../templates/poly_cols.csv")
pt_cols <- read.csv("../../templates/pt_cols.csv")

# Import plot metadata
plot_meta <- read.csv(file.path(indir, "ParacouDescription.csv"))

# # Extract subplot corners
# subplot_corners <- plot_meta %>% 
#   pivot_longer(
#     cols = starts_with("SubPlotL"),
#     names_to = c(".value", "corner_id"),
#     names_pattern = "SubPlot(Lat|Lon)(SW|SE|NE|NW)") %>% 
#   dplyr::select(
#     plot_id = Plot,
#     subplot_id = SubPlot,
#     longitude = Lon,
#     latitude = Lat)
# 
# subplot_corners_sf <- st_as_sf(subplot_corners, coords = c("longitude", "latitude")) 

# Extract plot corners
plot_corners <- plot_meta %>% 
  pivot_longer(
    cols = starts_with("PlotL"),
    names_to = c(".value", "corner_id"),
    names_pattern = "Plot(Lat|Lon)(SW|SE|NE|NW)") %>% 
  dplyr::select(
    plot_id = Plot,
    corner_id,
    area_reported_ha = PlotArea,
    longitude = Lon,
    latitude = Lat) %>% 
  filter(plot_id != "17(Arbocel)") %>% 
  distinct()

# plot_origin_corner <- plot_meta %>% 
#   mutate(PlotRefCorner = gsub("SO", "SW", trimws(PlotRefCorner))) %>% 
#   dplyr::select(
#     plot_id = Plot,
#     plot_origin_corner_id = PlotRefCorner) %>% 
#   filter(plot_id != "17(Arbocel)") %>% 
#   distinct()
# all(plot_origin_corner$plot_origin_corner_id == "SW")
# All plots have XY origin in SW corne corner.

# Extract reported plot areas
plot_areas <- plot_corners %>% 
  dplyr::select(plot_id, area_reported_ha) %>% 
  distinct()

# Create plot corner sf 
pt <- plot_corners %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(., crs = 32622) %>%  # UTM 22N
  mutate(
    site_id,
    x_rel_m = case_when(
      corner_id %in% c("SW", "NW") ~ 0,
      corner_id %in% c("SE", "NE") & area_reported_ha == 6.25 ~ 250,
      corner_id %in% c("SE", "NE") & area_reported_ha == 25 ~ 500,
      TRUE ~ NA_real_),
    y_rel_m = case_when(
      corner_id %in% c("SW", "SE") ~ 0,
      corner_id %in% c("NW", "NE") & area_reported_ha == 6.25 ~ 250,
      corner_id %in% c("NW", "NE") & area_reported_ha == 25 ~ 500,
      TRUE ~ NA_real_)) %>% 
  dplyr::select(-area_reported_ha) %>% 
  relocate(geometry, .after = last_col()) %>% 
  relocate(site_id)

# Create polygons
poly <- pt %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_convex_hull() %>% 
  ungroup() %>% 
  left_join(., plot_areas, by = "plot_id") %>% 
  mutate(perim_reported_m = NA_real_) %>% 
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

