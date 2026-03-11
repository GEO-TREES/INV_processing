# Split plots into quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Clean polygon data
pt_clean <- plot_pt %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() 

# Extract plot corners
plot_check <- check_plot_coord(
  corner_data = pt_clean,
  longlat = c("X", "Y"),  
  rel_coord = c("x_rel_m", "y_rel_m"),
  plot_ID = "plot_id",
  trust_GPS_corners = TRUE,
  draw_plot = FALSE,
  tree_data = stem, 
  tree_coords = c("x_rel_m", "y_rel_m"),
  tree_plot_ID = "plot_id")

# Divide plot
plot_divide <- divide_plot(
  corner_data = plot_check$corner_coord,
  rel_coord = c("x_rel", "y_rel"),
  proj_coord = c("x_proj", "y_proj"),
  longlat = NULL,
  grid_size = param$quad_dim,
  grid_tol = 1,
  origin = NULL,
  tree_data = plot_check$tree_data,
  tree_coords = c("x_rel", "y_rel"),
  corner_plot_ID = "plot_ID",
  tree_plot_ID = "plot_ID",
  sd_coord = NULL, 
  n = 100)

# Create quadrat polygons
quad_pt <- plot_divide$sub_corner_coord %>% 
  st_as_sf(., coords = c("x_proj", "y_proj"), crs = st_crs(plot_pt)) %>% 
  rename(
    plot_id = plot_ID, 
    quadrat_id = subplot_ID,
    x_rel_m = x_rel,
    y_rel_m = y_rel)

quad_poly <- quad_pt %>% 
  group_by(plot_id, quadrat_id) %>%
  summarise() %>% 
  st_convex_hull() 

stem_pt <- plot_divide$tree_data %>% 
  filter(!is.na(x_proj), !is.na(y_proj)) %>% 
  st_as_sf(., coords = c("x_proj", "y_proj"), crs = st_crs(plot_pt)) %>% 
  dplyr::select(
    record_id,
    quadrat_id = subplot_ID)

# Write quadrat points to file
st_write(quad_pt, file.path(outdir, "quad_pt.gpkg"), delete_dsn = TRUE)

# Write quadrat polygons to file
st_write(quad_poly, file.path(outdir, "quad_poly.gpkg"), delete_dsn = TRUE)

# Write global stem coordinates to file
st_write(stem_pt, file.path(outdir, "stem_pt.gpkg"), delete_dsn = TRUE)
