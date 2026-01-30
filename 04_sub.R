# Split plots into subplots
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(sf)
library(BIOMASS)
library(data.table)

source("./func.R")

# Define directories
# outdir <- "./dat/sites/Panama Canal/04_sub"

# Import data 
# pt <- read_sf("./dat/sites/Panama Canal/01_plot/plot_pt.gpkg")
# s <- read.csv("./dat/sites/Panama Canal/02_stem/stem.csv")

# Clean polygon data
pt_clean <- plot_pt %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() 

# Extract plot corners
check_plot <- check_plot_coord(
  corner_data = pt_clean,
  proj_coord = c("X", "Y"),  
  rel_coord = c("x_rel", "y_rel"),
  plot_ID = "plot_id",
  trust_GPS_corners = TRUE,
  draw_plot = FALSE,
  tree_data = stem, 
  tree_coords = c("x_rel", "y_rel"),
  tree_plot_ID = "plot_id")

# Define subplot dimensions
sub_dim <- c(50, 50)

# Divide plot
align_vec <- c("bottomleft", "bottomright", "topright", "topleft", "centre")
sub_list <- lapply(align_vec, function(i) {
  out <- divide_plot2(
    corner_data = check_plot$corner_coord,
    rel_coord = c("x_rel","y_rel"),
    proj_coord = c("x_proj","y_proj"),
    corner_plot_ID = "plot_ID",
    grid_size = sub_dim, 
    grid_tol = 1,
    origin = i,
    tree_data = check_plot$tree_data, 
    tree_coords = c("x_rel", "y_rel"),
    tree_plot_ID = "plot_ID")
  out[[1]]$subplot_ID <- paste(out[[1]]$subplot_ID, i, sep = "_")
  out[[2]]$subplot_ID <- paste(out[[2]]$subplot_ID, i, sep = "_")
  out[[2]] <- out[[2]][out[[2]]$subplot_ID != paste0("NA_", i),]
  out
})
names(sub_list) <- align_vec

out_list <- lapply(names(sub_list), function(i) { 
  # Create subplot corner sf points
  sub_pt <- st_as_sf(sub_list[[i]]$sub_corner_coord, 
    coords = c("x_proj", "y_proj"), crs = st_crs(pt))

  # Create subplot polygons
  sub_poly <- sub_pt %>% 
    group_by(corner_plot_ID, subplot_ID) %>% 
    summarise() %>% 
    st_convex_hull() 

  # Extract stem coordinates
  stem_pt <- sub_list[[i]]$tree_data %>% 
    dplyr::select(
      measurement_id,
      x_proj,
      y_proj,
      subplot_ID) %>% 
    st_as_sf(., coords = c("x_proj", "y_proj"), crs = st_crs(pt))

  # Rename columns for output
  sub_pt_out <- sub_pt %>% 
    rename(
      plot_id = corner_plot_ID,
      subplot_id = subplot_ID)

  sub_poly_out <- sub_poly %>% 
    rename(
      plot_id = corner_plot_ID,
      subplot_id = subplot_ID)

  stem_pt_out <- stem_pt %>% 
    rename(subplot_id = subplot_ID)

  return(list(sub_pt_out, sub_poly_out, stem_pt_out))
})
names(out_list) <- names(sub_list)

# Combine lists of polygons and points
sub_pt_all <- bind_rows(lapply(out_list, "[[", 1))

sub_poly_all <- bind_rows(lapply(out_list, "[[", 2))

# Combine lists of stem coordinates
stem_pt_only <- bind_rows(lapply(out_list, "[[", 3)) %>% 
  group_by(measurement_id) %>% 
  mutate(row = row_number()) %>%
  filter(row == 1) %>% 
  dplyr::select(measurement_id) 

stem_sub_only <- bind_rows(lapply(out_list, "[[", 3)) %>% 
  st_drop_geometry() %>% 
  group_by(measurement_id) %>% 
  summarise(subplot_id_vec = paste(subplot_id, collapse = ";"))

stem_pt <- right_join(stem_pt_only, stem_sub_only) %>% 
  relocate(measurement_id, subplot_id_vec)

# Write subplot points to file
st_write(sub_pt_all, file.path(outdir, "sub_pt.gpkg"), delete_dsn = TRUE)

# Write subplot polygons to file
st_write(sub_poly_all, file.path(outdir, "sub_poly.gpkg"), delete_dsn = TRUE)

# Write global stem coordinates to file
st_write(stem_pt, file.path(outdir, "stem_pt.gpkg"), delete_dsn = TRUE)
