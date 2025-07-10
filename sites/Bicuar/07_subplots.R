# Split plots into subplots
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(sf)
library(BIOMASS)

# Define directories
outdir <- "../../dat/sites/Bicuar/07_subplots"

# Import data 
p <- read_sf("../../dat/sites/Bicuar/01_polys/pts.gpkg")

# Clean polygon data
p_clean <- p %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry()

# Extract plot corners
check_plot <- check_plot_coord(
  corner_data = p_clean,
  proj_coord = c("X", "Y"),  
  rel_coord = c("x_rel", "y_rel"),
  plot_ID = "Plot_name",
  trust_GPS_corners = TRUE,
  draw_plot = FALSE)

# Define subplot dimensions
subplot_dim <- c(50, 50)

# Divide plot
subplots <- divide_plot(
  corner_data = check_plot$corner_coord,
  rel_coord = c("x_rel","y_rel"),
  proj_coord = c("x_proj","y_proj"),
  corner_plot_ID = "plot_ID",
  grid_size = subplot_dim, 
  grid_tol = 1,
  centred_grid = TRUE)

# Create subplot corner sf points
pts_sub <- st_as_sf(subplots, coords = c("x_proj", "y_proj"), crs = st_crs(p))

# Create subplot polygons
polys_sub <- pts_sub %>% 
  group_by(corner_plot_ID, subplot_ID) %>% 
  summarise() %>% 
  st_convex_hull() 

# Write subplot points to file
st_write(pts_sub, file.path(outdir, "pts_sub.gpkg"), delete_dsn = TRUE)

# Write subplot polygons to file
st_write(polys_sub, file.path(outdir, "polys_sub.gpkg"), delete_dsn = TRUE)

