# Split plots into quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Find local UTM
plot_pt_coord <- st_coordinates(plot_pt)
plot_pt_utm <- getUTM(plot_pt_coord[,1], plot_pt_coord[,2])

# Assign most frequent EPSG to every row within each plot
plot_pt_utm <- ave(plot_pt_utm, plot_pt$plot_id, FUN = function(x) {
  as.numeric(names(which.max(table(x))))
})

# Split multiple UTM zones
plot_pt_list <- split(plot_pt, plot_pt_utm)

# For each UTM zone
out_list <- lapply(names(plot_pt_list), function(x) {

  # Clean polygon data
  pt_clean <- plot_pt_list[[x]] %>% 
    st_transform(., as.numeric(x)) %>% 
    cbind(., st_coordinates(.)) %>% 
    st_drop_geometry() 

  # Extract plot corners
  plot_check <- check_plot_coord(
    corner_data = pt_clean,
    proj_coord = c("X", "Y"),  
    rel_coord = c("x_rel_m", "y_rel_m"),
    plot_ID = "plot_id",
    trust_GPS_corners = TRUE,
    draw_plot = FALSE,
    tree_data = stem[stem$plot_id %in% pt_clean$plot_id,], 
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

  # Create plot polygons 
  plot_poly <- plot_check$polygon %>% 
    st_set_crs(as.numeric(x)) %>% 
    mutate(    
      site_id = param$site_id,
      acquisition_id = param$acquisition_id, 
      plot_id = unique(pt_clean$plot_id),
      .before = everything()) %>%
    rename(geometry = x)

  # Create quadrat polygons
  quad_pt <- plot_divide$sub_corner_coord %>% 
    st_as_sf(., coords = c("x_proj", "y_proj"), crs = as.numeric(x)) %>% 
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
    filter(!is.na(x_proj), !is.na(y_proj)) %>% 
    st_as_sf(., coords = c("x_proj", "y_proj"), crs = as.numeric(x)) %>% 
    dplyr::select(
      record_id,
      quadrat_id = subplot_ID)

  # Convert back to WGS84
  plot_poly <- st_transform(plot_poly, 4326)
  quad_pt <- st_transform(quad_pt, 4326)
  quad_poly <- st_transform(quad_poly, 4326)
  stem_pt <- st_transform(stem_pt, 4326)

  # Return
  return(list(
    "plot_poly" = plot_poly,
    "quad_pt" = quad_pt, 
    "quad_poly" = quad_poly, 
    "stem_pt" = stem_pt))
})

# Join multiple items
plot_poly <- bind_rows(lapply(out_list, "[[", "plot_poly"))
quad_pt <- bind_rows(lapply(out_list, "[[", "quad_pt"))
quad_poly <- bind_rows(lapply(out_list, "[[", "quad_poly"))
stem_pt <- bind_rows(lapply(out_list, "[[", "stem_pt"))

# Write plot polygons to file
st_write(plot_poly, file.path(outdir, "plot_poly.gpkg"), delete_dsn = TRUE)

# Write quadrat points to file
st_write(quad_pt, file.path(outdir, "quad_pt.gpkg"), delete_dsn = TRUE)

# Write quadrat polygons to file
st_write(quad_poly, file.path(outdir, "quad_poly.gpkg"), delete_dsn = TRUE)

# Write global stem coordinates to file
st_write(stem_pt, file.path(outdir, "stem_pt.gpkg"), delete_dsn = TRUE)
