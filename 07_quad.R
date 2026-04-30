# Split plots into quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# entirely deterministic for now, we would like to include uncertainties on 
# coordinates using plot_polygons (L2) product. To be computed with BIOMASS::divide_plot 
# through "sd_coord" argument, it needs to be transformed in a data frame containing (for each plot) 
# the average standard deviation of the GPS measurements for each corner on the X and Y axes

# Select only relevant columns from stems dataframe
# Filter to only plots with spatial coordinates
stem_sel <- stem %>% 
  dplyr::select(record_id, plot_id, x_rel_m, y_rel_m) %>% 
  filter(
    plot_id %in% plot_pt$plot_id,
    !is.na(x_rel_m),
    !is.na(y_rel_m))

# Extract plot corners
plot_check <- plotPolygonFit(
  point_data = plot_pt,
  method = "procrustes",
  rel_col = c("x_rel_m", "y_rel_m"),
  proj_col = c("rover_easting_utm_m", "rover_northing_utm_m"),
  lonlat_col = NULL,
  corner_col = "corner",
  plot_col = "plot_id",
  max_dist = 10,
  rm_outliers = TRUE,
  tree_data = stem_sel,
  tree_rel_col = c("x_rel_m", "y_rel_m"),
  tree_plot_col = "plot_id",
  raster = NULL,
  shapefile = NULL,
  prop_tree = NULL,
  threshold_tree = NULL, 
  draw_plot = FALSE,
  ask = FALSE)

# Divide plot
plot_divide <- BIOMASS::divide_plot(
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

# Create plot polygons 
plot_poly <- plot_check$polygon %>% 
  rename(plot_id = plot_ID) %>% 
  st_set_crs(unique(plot_pt$crs_epsg)) %>% 
  mutate(    
    site_id = param$site_id,
    acquisition_id = param$acquisition_id, 
    .before = everything())

# Create quadrat polygons
quad_pt <- plot_divide$sub_corner_coord %>% 
  mutate(
    x_utm = x_proj,
    y_utm = y_proj) %>% 
  st_as_sf(., coords = c("x_proj", "y_proj"), crs = unique(plot_pt$crs_epsg)) %>% 
  mutate(    
    site_id = param$site_id,
    acquisition_id = param$acquisition_id, 
    .before = everything()) %>% 
  rename(
    plot_id = plot_ID, 
    quadrat_id = subplot_ID,
    x_rel_m = x_rel,
    y_rel_m = y_rel)

quad_poly <- quad_pt %>% 
  group_by(site_id, acquisition_id, plot_id, quadrat_id) %>%
  summarise() %>% 
  st_convex_hull() %>%
  ungroup()

# Create stem points
stem_pt <- plot_divide$tree_data %>% 
  mutate(
    x_utm = x_proj,
    y_utm = y_proj) %>% 
  filter(!is.na(x_proj), !is.na(y_proj)) %>% 
  st_as_sf(., coords = c("x_proj", "y_proj"), crs = unique(plot_pt$crs_epsg)) %>% 
  dplyr::select(
    record_id,
    quadrat_id = subplot_ID,
    x_utm, y_utm)

# Write plot polygons to file
st_write(plot_poly, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write plot corner points
st_write(plot_pt, file.path(outdir, "plot_corner.gpkg"), delete_dsn = TRUE)

# Write quadrat polygons to file
st_write(quad_poly, file.path(outdir, "quad_poly.gpkg"), delete_dsn = TRUE)

# Write quadrat points to file
st_write(quad_pt, file.path(outdir, "quad_pt.gpkg"), delete_dsn = TRUE)

# Write global stem coordinates to file
st_write(stem_pt, file.path(outdir, "stem_pt.gpkg"), delete_dsn = TRUE)

