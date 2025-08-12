# Split plots into subplots
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(sf)
library(BIOMASS)
library(data.table)

# Define directories
# outdir <- "./dat/sites/Panama Canal/04_subplots"

# Import data 
# pts <- read_sf("./dat/sites/Panama Canal/01_polys/pts.gpkg")
# stems <- read.csv("./dat/sites/Panama Canal/02_stem_fmt/stems.csv")

# Clean polygon data
pts_clean <- pts %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() 

# Extract plot corners
check_plot <- check_plot_coord(
  corner_data = pts_clean,
  proj_coord = c("X", "Y"),  
  rel_coord = c("x_rel", "y_rel"),
  plot_ID = "plot_id",
  trust_GPS_corners = TRUE,
  draw_plot = FALSE,
  tree_data = stems, 
  tree_coords = c("x_rel", "y_rel"),
  tree_plot_ID = "plot_id")

# Define subplot dimensions
subplot_dim <- c(50, 50)

# Divide plot
align_vec <- c("bottomleft", "bottomright", "topright", "topleft", "centre")
subplots_list <- lapply(align_vec, function(i) {
  out <- divide_plot2(
    corner_data = check_plot$corner_coord,
    rel_coord = c("x_rel","y_rel"),
    proj_coord = c("x_proj","y_proj"),
    corner_plot_ID = "plot_ID",
    grid_size = subplot_dim, 
    grid_tol = 1,
    origin = i,
    tree_data = check_plot$tree_data, 
    tree_coords = c("x_rel", "y_rel"),
    tree_plot_ID = "plot_ID")
  out[[1]]$subplot_ID <- paste(out[[1]]$subplot_ID, i, sep = "_")
  out[[2]]$subplot_ID <- paste(out[[2]]$subplot_ID, i, sep = "_")
  out
})
names(subplots_list) <- align_vec

out_list <- lapply(names(subplots_list), function(i) { 
  # Create subplot corner sf points
  pts_sub <- st_as_sf(subplots_list[[i]]$sub_corner_coord, 
    coords = c("x_proj", "y_proj"), crs = st_crs(pts))

  # Create subplot polygons
  polys_sub <- pts_sub %>% 
    group_by(corner_plot_ID, subplot_ID) %>% 
    summarise() %>% 
    st_convex_hull() 

  # Extract stem coordinates
  stem_coords <- subplots_list[[i]]$tree_data %>% 
    dplyr::select(
      measurement_id,
      x_proj,
      y_proj,
      subplot_ID) %>% 
    st_as_sf(., coords = c("x_proj", "y_proj"), crs = st_crs(pts))

  # Rename columns for output
  pts_sub_out <- pts_sub %>% 
    rename(
      plot_id = corner_plot_ID,
      subplot_id = subplot_ID)

  polys_sub_out <- polys_sub %>% 
    rename(
      plot_id = corner_plot_ID,
      subplot_id = subplot_ID)

  stem_coords_out <- stem_coords %>% 
    rename(subplot_id = subplot_ID)

  return(list(pts_sub_out, polys_sub_out, stem_coords_out))
})
names(out_list) <- names(subplots_list)

# Write data to files
lapply(names(out_list), function(i) {
  # Write subplot points to file
  st_write(out_list[[i]][[1]], file.path(outdir, paste0("pts_sub_", i, ".gpkg")), delete_dsn = TRUE)

  # Write subplot polygons to file
  st_write(out_list[[i]][[2]], file.path(outdir, paste0("polys_sub_", i, ".gpkg")), delete_dsn = TRUE)

  # Write global stem coordinates to file
  st_write(out_list[[i]][[3]], file.path(outdir, paste0("stem_coords_", i, ".gpkg")), delete_dsn = TRUE)
})

