# Split plots into quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Plot polygons and quadrats are fitted from GNSS measurements of plot
# corners, using the method in `polygon_method`. Uncertainty in the corner
# coordinates is not propagated. This needs repeated GNSS measurements of
# each corner, to estimate `sd_coord` for BIOMASS::divide_plot()

# Select only relevant columns from stems dataframe
# Filter to only plots with spatial coordinates
# Multi-stemmed trees, see `treeEquiv()` in ./func.R, are assigned to the
# quadrat containing the centroid of their stems, so `x_quad` and `y_quad`
# are used to assign quadrats, while stem locations are kept
stem_sel <- stem %>%
  left_join(., treeEquiv(stem, record_fil)[, c("record_id", "tree_key")],
    by = "record_id") %>%
  dplyr::select(record_id, plot_id, tree_key, x_rel_m, y_rel_m) %>%
  filter(
    plot_id %in% plot_pt$plot_id,
    !is.na(x_rel_m),
    !is.na(y_rel_m)) %>%
  group_by(tree_key) %>%
  mutate(
    x_quad = mean(x_rel_m),
    y_quad = mean(y_rel_m)) %>%
  ungroup() %>%
  dplyr::select(-tree_key)

# Points must share one projected CRS. If plots span more than one UTM zone,
# transform all points to the most common zone
crs_site <- as.integer(names(which.max(table(plot_pt$crs_epsg))))
pt_fit <- plot_pt
if (length(unique(pt_fit$crs_epsg)) > 1) {
  message("Plot points span more than one UTM zone, transforming to EPSG:", crs_site)
  for (i in setdiff(unique(pt_fit$crs_epsg), crs_site)) {
    sel <- pt_fit$crs_epsg == i & 
      !is.na(pt_fit$rover_easting_utm_m) & !is.na(pt_fit$rover_northing_utm_m)
    xy <- st_coordinates(st_transform(
      st_as_sf(pt_fit[sel, ], 
        coords = c("rover_easting_utm_m", "rover_northing_utm_m"), crs = i), 
      crs_site))
    pt_fit$rover_easting_utm_m[sel] <- xy[, 1]
    pt_fit$rover_northing_utm_m[sel] <- xy[, 2]
  }
  pt_fit$crs_epsg <- crs_site
}

# Extract plot corners
plot_check <- plotPolygonFit(
  point_data = pt_fit,
  method = param$polygon_method,
  rel_col = c("x_rel_m", "y_rel_m"),
  proj_col = c("rover_easting_utm_m", "rover_northing_utm_m"),
  lonlat_col = NULL,
  corner_col = "corner",
  plot_col = "plot_id",
  max_dist = 10,
  rm_outliers = TRUE,
  tree_data = if (nrow(stem_sel) > 0) stem_sel else NULL,
  tree_rel_col = c("x_rel_m", "y_rel_m"),
  tree_plot_col = if (nrow(stem_sel) > 0) "plot_id" else NULL,
  raster = NULL,
  shapefile = NULL,
  prop_tree = NULL,
  threshold_tree = NULL, 
  draw_plot = FALSE,
  ask = FALSE)

# Divide plot, assigning stems to quadrats using the centroid of their tree.
# divide_plot() renames `tree_coords` to `x_rel` and `y_rel`, so the stem
# coordinates are removed
tree_quad <- if (nrow(stem_sel) > 0) {
  dplyr::select(plot_check$tree_data, -x_rel, -y_rel)
} else {
  NULL
}

plot_divide <- BIOMASS::divide_plot(
  corner_data = plot_check$corner_coord,
  rel_coord = c("x_rel", "y_rel"),
  proj_coord = c("x_proj", "y_proj"),
  longlat = NULL,
  grid_size = param$quad_dim,
  grid_tol = 0.1,  # Max. proportion of plot area excluded from the grid
  origin = NULL,
  tree_data = tree_quad,
  tree_coords = if (nrow(stem_sel) > 0) c("x_quad", "y_quad") else NULL,
  corner_plot_ID = "plot_ID",
  tree_plot_ID = if (nrow(stem_sel) > 0) "plot_ID" else NULL,
  sd_coord = NULL, 
  n = 100)

# Create plot polygons 
plot_poly <- plot_check$polygon %>% 
  rename(plot_id = plot_ID) %>% 
  st_set_crs(crs_site) %>% 
  mutate(    
    site_id = param$site_id,
    acquisition_id = param$acquisition_id, 
    .before = everything())

# Create quadrat polygons
quad_pt <- plot_divide$sub_corner_coord %>% 
  mutate(
    x_utm = x_proj,
    y_utm = y_proj) %>% 
  st_as_sf(., coords = c("x_proj", "y_proj"), crs = crs_site) %>% 
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
# Stems without coordinates are not included
if (nrow(stem_sel) > 0) {
  stem_pt <- plot_divide$tree_data %>%
    mutate(
      x_utm = x_proj,
      y_utm = y_proj) %>%
    filter(!is.na(x_proj), !is.na(y_proj)) %>%
    st_as_sf(., coords = c("x_proj", "y_proj"), crs = crs_site) %>%
    dplyr::select(
      record_id,
      quadrat_id = subplot_ID,
      x_utm, y_utm)
} else {
  stem_pt <- st_sf(
    record_id = integer(0),
    quadrat_id = character(0),
    x_utm = numeric(0),
    y_utm = numeric(0),
    geometry = st_sfc(crs = crs_site))
}

# Write plot polygons to file
st_write(plot_poly, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write quadrat polygons to file
st_write(quad_poly, file.path(outdir, "quad_poly.gpkg"), delete_dsn = TRUE)

# Write quadrat points to file
st_write(quad_pt, file.path(outdir, "quad_pt.gpkg"), delete_dsn = TRUE)

# Write global stem coordinates to file
st_write(stem_pt, file.path(outdir, "stem_pt.gpkg"), delete_dsn = TRUE)

