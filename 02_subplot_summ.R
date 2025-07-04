# Create subplot-level summary statistics from tree inventory data
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-06-18

# Define directories
indir <- "../dat/clean/panama"
outdir <- "../dat/processed/panama"

# Define subplot resolution
subplot_dim <- c(50, 50)

# Packages
library(dplyr)
library(sf)
library(BIOMASS)

# Source functions
source("./func.R")

# Import data
p <- read_sf(file.path(indir, "polys_pts.gpkg"))

s <- read.csv(file.path(indir, "stems_calc.csv"))

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

# Divide plot
subplots <- divide_plot(
  corner_data = check_plot$corner_coord,
  rel_coord = c("x_rel","y_rel"),
  proj_coord = c("x_proj","y_proj"),
  corner_plot_ID = "plot_ID",
  grid_size = subplot_dim, 
  grid_tol = 1,
  centred_grid = FALSE,
  tree_data = s,
  tree_coords = c("x_grid", "y_grid"),
  tree_plot_ID = "Plot_name")

subplot_summ <- subplot_summary(
  subplots,
  value = c("agb", "diam", "ba"),
  fun = list("agb" = sum, "diam" = mean, "ba" = sum),
  per_ha = c(TRUE, FALSE, TRUE)) 

# Write sf object
write.csv(subplot_summ$tree_summary, 
  file.path(outdir, "subplot_summ.csv"), row.names = FALSE)
