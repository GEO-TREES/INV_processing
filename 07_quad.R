# Split plots into quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-21

# Plot polygons and quadrats are fitted from GNSS measurements of plot
# corners, using the method in `polygon_method`. Uncertainty in the corner
# coordinates is not propagated. This needs repeated GNSS measurements of
# each corner, to estimate `sd_coord` for BIOMASS::divide_plot()

# Select only relevant columns from stems dataframe
# Filter to only plots with spatial coordinates
# Multi-stemmed trees, see `treeEquiv()` in ./func.R, are assigned to the
# quadrat containing the centroid of their stems, so `x_quad` and `y_quad`
# are used to assign quadrats, while stem locations are kept
tree_include <- if (param$multi_stem_method == "equivalent") record_agbd else character(0)
stem_sel <- stem %>%
  left_join(., treeEquiv(stem, tree_include)[, c("record_id", "tree_key")],
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
  max_dist = param$corner_outlier_dist_m,
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

# Divide plots into quadrats, assigning stems to quadrats using the centroid
# of their tree. divide_plot() renames `tree_coords` to `x_rel` and `y_rel`,
# so the stem coordinates are removed
tree_quad <- if (nrow(stem_sel) > 0) {
  dplyr::select(plot_check$tree_data, -x_rel, -y_rel)
} else {
  NULL
}

# Each plot is divided separately, with the quadrat grid aligned by 
# `quad_origin`: "centre" centres the grid in the plot, "corner" starts it
# at the origin corner of the plot, and [x, y] starts it x and y m from the
# origin corner. Where the plot dimensions are not a multiple of the quadrat
# dimensions, strips along the edges of the plot are left out, e.g. with 
# "centre", 25 m strips along each edge of a 250 x 250 m plot divided into
# 100 x 100 m quadrats. Up to `quad_grid_tol` of the area of each plot can be
# left out. Stems in these strips are excluded from quadrat summaries
corner_coord <- as.data.frame(plot_check$corner_coord)
divide_list <- lapply(split(corner_coord, corner_coord$plot_ID), function(cc) {
  x_rng <- range(cc$x_rel)
  y_rng <- range(cc$y_rel)
  origin <- if (identical(param$quad_origin, "centre")) {
    c(x_rng[1] + (diff(x_rng) %% param$quad_dim[1]) / 2,
      y_rng[1] + (diff(y_rng) %% param$quad_dim[2]) / 2)
  } else if (identical(param$quad_origin, "corner")) {
    c(x_rng[1], y_rng[1])
  } else {
    c(x_rng[1], y_rng[1]) + param$quad_origin
  }
  td <- if (!is.null(tree_quad)) {
    tree_quad[tree_quad$plot_ID == cc$plot_ID[1], , drop = FALSE]
  } else {
    NULL
  }
  if (!is.null(td) && nrow(td) == 0) td <- NULL

  # divide_plot() warns where the plot dimensions are not a multiple of the
  # quadrat dimensions, which is expected. The area left out is reported 
  # below
  withCallingHandlers(
    BIOMASS::divide_plot(
      corner_data = cc,
      rel_coord = c("x_rel", "y_rel"),
      proj_coord = c("x_proj", "y_proj"),
      longlat = NULL,
      grid_size = param$quad_dim,
      grid_tol = param$quad_grid_tol,
      origin = origin,
      tree_data = td,
      tree_coords = if (!is.null(td)) c("x_quad", "y_quad") else NULL,
      corner_plot_ID = "plot_ID",
      tree_plot_ID = if (!is.null(td)) "plot_ID" else NULL,
      sd_coord = NULL, 
      n = 100),
    warning = function(w) {
      if (grepl("is not a multiple of|could not be assigned to a subplot", 
        conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    })
})
plot_divide <- list(
  sub_corner_coord = bind_rows(lapply(divide_list, function(x) {
    as.data.frame(x$sub_corner_coord)
  })),
  tree_data = bind_rows(lapply(divide_list, function(x) {
    if (is.null(x$tree_data)) NULL else as.data.frame(x$tree_data)
  })))

# Stems outside the quadrat grid
# Stems whose tree centroid is outside the quadrat grid, by up to
# `outside_max_dist_m`, are assigned to the nearest quadrat. Others are
# excluded from quadrat summaries. Stems outside the plot are written to a 
# check file, so that raw data can be corrected if needed. Stems inside the
# plot, in strips left out of the quadrat grid, are not
if (nrow(stem_sel) > 0) {
  plot_divide$tree_data$grid_excl <- FALSE
  tree_dat <- plot_divide$tree_data
  quad_ext <- plot_divide$sub_corner_coord %>%
    group_by(plot_ID, subplot_ID) %>%
    summarise(
      x_min = min(x_rel), x_max = max(x_rel),
      y_min = min(y_rel), y_max = max(y_rel),
      .groups = "drop")
  plot_ext <- corner_coord %>%
    group_by(plot_ID) %>%
    summarise(
      x_min = min(x_rel), x_max = max(x_rel),
      y_min = min(y_rel), y_max = max(y_rel),
      .groups = "drop")

  # Distance from tree centroid to nearest quadrat. `x_rel` and `y_rel` in
  # the output of divide_plot() are the tree centroid
  out_idx <- which(is.na(tree_dat$subplot_ID))
  quad_near <- lapply(out_idx, function(i) {
    q <- quad_ext[quad_ext$plot_ID == tree_dat$plot_ID[i], ]
    d <- sqrt(
      pmax(q$x_min - tree_dat$x_rel[i], 0, tree_dat$x_rel[i] - q$x_max)^2 +
      pmax(q$y_min - tree_dat$y_rel[i], 0, tree_dat$y_rel[i] - q$y_max)^2)
    data.frame(quadrat_nearest = q$subplot_ID[which.min(d)], dist_m = min(d))
  })
  quad_near <- bind_rows(quad_near,
    data.frame(quadrat_nearest = character(0), dist_m = numeric(0)))

  # Whether the tree centroid is inside the plot
  pe <- plot_ext[match(tree_dat$plot_ID[out_idx], plot_ext$plot_ID), ]
  in_plot <- tree_dat$x_rel[out_idx] >= pe$x_min & 
    tree_dat$x_rel[out_idx] <= pe$x_max &
    tree_dat$y_rel[out_idx] >= pe$y_min & 
    tree_dat$y_rel[out_idx] <= pe$y_max

  stem_outside <- data.frame(
    record_id = tree_dat$record_id[out_idx],
    quadrat_nearest = quad_near$quadrat_nearest,
    dist_m = round(quad_near$dist_m, 2)) %>%
    mutate(
      action = case_when(
        dist_m <= param$outside_max_dist_m ~ "assigned to nearest quadrat",
        in_plot ~ "excluded, in plot outside quadrat grid",
        TRUE ~ "excluded"))

  # Assign stems to nearest quadrat
  assign_idx <- out_idx[stem_outside$action == "assigned to nearest quadrat"]
  plot_divide$tree_data$subplot_ID[assign_idx] <- stem_outside$quadrat_nearest[
    stem_outside$action == "assigned to nearest quadrat"]

  # Mark stems in strips left out of the quadrat grid
  grid_excl_idx <- out_idx[stem_outside$action == "excluded, in plot outside quadrat grid"]
  plot_divide$tree_data$grid_excl[grid_excl_idx] <- TRUE
  if (length(grid_excl_idx) > 0) {
    message(length(grid_excl_idx), " stems in strips of plots left out of the ",
      "quadrat grid, excluded from quadrat summaries")
  }
  stem_outside <- filter(stem_outside, action != "excluded, in plot outside quadrat grid")

  if (nrow(stem_outside) > 0) {
    message(nrow(stem_outside), " stems outside the quadrat grid: ",
      sum(stem_outside$action != "excluded"), " assigned to the nearest quadrat, ",
      sum(stem_outside$action == "excluded"), " excluded. See ",
      file.path(report_dir, "stem_outside.csv"))
  }

  # Write stems outside the quadrat grid to file, to check raw data
  stem_outside <- stem %>%
    dplyr::select(record_id, plot_id, tree_id, stem_id, x_rel_m, y_rel_m,
      diam_cm, code) %>%
    inner_join(., stem_outside, by = "record_id") %>%
    arrange(desc(dist_m))
  write.csv(stem_outside, file.path(report_dir, "stem_outside.csv"), row.names = FALSE)
}

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
      grid_excl,
      x_utm, y_utm)
} else {
  stem_pt <- st_sf(
    record_id = integer(0),
    quadrat_id = character(0),
    grid_excl = logical(0),
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


# Summarise plot polygons. The area of the fitted polygon should be close to
# the area defined by the relative coordinates of the plot corners. A large
# difference suggests errors in the corner coordinates. `grid_excl_pct` is 
# the percentage of the plot area left out of the quadrat grid
corner_rel <- pt_fit %>%
  filter(corner %in% TRUE) %>%
  distinct(plot_id, x_rel_m, y_rel_m)
area_rel <- vapply(split(corner_rel, corner_rel$plot_id), function(x) {
  as.numeric(st_area(st_convex_hull(st_multipoint(
    as.matrix(x[, c("x_rel_m", "y_rel_m")]))))) / 10000
}, numeric(1))

outliers <- if (is.null(plot_check$outliers)) {
  data.frame(plot_ID = character(0), x_proj = numeric(0), y_proj = numeric(0))
} else {
  plot_check$outliers
}

poly_check <- data.frame(
  plot_id = as.character(plot_poly$plot_id),
  area_ha_poly = as.numeric(st_area(plot_poly)) / 10000) %>%
  mutate(
    area_ha_rel = unname(area_rel[plot_id]),
    area_diff_pct = round(100 * (area_ha_poly - area_ha_rel) / area_ha_rel, 1),
    n_gnss = as.vector(table(pt_fit$plot_id)[plot_id]),
    n_gnss_outlier = as.vector(table(factor(outliers$plot_ID, levels = plot_id))),
    n_quadrat = as.vector(table(factor(quad_poly$plot_id, levels = plot_id))),
    area_ha_quad = as.vector(tapply(as.numeric(st_area(quad_poly)) / 10000,
      factor(quad_poly$plot_id, levels = plot_id), sum)),
    grid_excl_pct = round(100 * (1 - area_ha_quad / area_ha_poly), 1)) %>%
  arrange(plotFactor(plot_id))
if (nrow(stem_sel) > 0) {
  stem_grid_excl <- stem_sel$plot_id[stem_sel$record_id %in% 
    plot_divide$tree_data$record_id[plot_divide$tree_data$grid_excl]]
  poly_check <- poly_check %>%
    mutate(
      n_stem_mapped = as.vector(table(factor(stem_sel$plot_id, levels = plot_id))),
      n_stem_outside = as.vector(table(factor(stem_outside$plot_id, levels = plot_id))),
      n_stem_grid_excl = as.vector(table(factor(stem_grid_excl, levels = plot_id))))
}
msgTable(poly_check, paste0("Plot polygons (", param$polygon_method, 
  " method). area_ha_rel is the area defined by the relative coordinates ",
  "of the plot corners. grid_excl_pct is the percentage of the plot area ",
  "left out of the quadrat grid:"))
write.csv(poly_check, file.path(outdir, "plot_poly_check.csv"), row.names = FALSE)

if (any(abs(poly_check$area_diff_pct) > 5, na.rm = TRUE)) {
  warning("Fitted polygon area differs from the area defined by the plot ",
    "corners by more than 5% in plots: ", 
    paste(poly_check$plot_id[abs(poly_check$area_diff_pct) > 5], collapse = ", "),
    ". Check plot corner coordinates, see ", 
    file.path(report_dir, "07_plot_poly.png"), call. = FALSE)
}

# Figure of plot polygons, quadrats and GNSS points. Coordinates are 
# relative to the south-west corner of the bounding box of each plot, so that
# plots can be shown side by side at the same scale
plot_xy <- polyCoords(plot_poly, "plot_id")
quad_xy <- polyCoords(quad_poly, "plot_id")
plot_origin <- plot_xy %>%
  group_by(plot_id) %>%
  summarise(x0 = min(X), y0 = min(Y), .groups = "drop")

gnss_xy <- pt_fit %>%
  filter(!is.na(rover_easting_utm_m), !is.na(rover_northing_utm_m)) %>%
  transmute(
    plot_id = as.character(plot_id), 
    X = rover_easting_utm_m, 
    Y = rover_northing_utm_m,
    type = ifelse(corner %in% TRUE, "GNSS point, corner", "GNSS point, other"))
outlier_xy <- transmute(outliers, plot_id = as.character(plot_ID), X = x_proj, 
  Y = y_proj, type = "Outlier, removed")
pt_xy <- bind_rows(gnss_xy, outlier_xy)

# Outline of the GNSS corners of each plot, from the mean of repeated
# measurements of each corner, ordered around the plot
corner_raw <- pt_fit %>%
  filter(corner %in% TRUE, !is.na(rover_easting_utm_m), 
    !is.na(rover_northing_utm_m)) %>%
  mutate(plot_id = as.character(plot_id)) %>%
  group_by(plot_id, x_rel_m, y_rel_m) %>%
  summarise(X = mean(rover_easting_utm_m), Y = mean(rover_northing_utm_m), 
    .groups = "drop") %>%
  group_by(plot_id) %>%
  filter(n() >= 3) %>%
  mutate(angle = atan2(y_rel_m - mean(y_rel_m), x_rel_m - mean(x_rel_m))) %>%
  ungroup() %>%
  arrange(plot_id, angle) %>%
  mutate(group = plot_id)

pt_type <- c(
  "GNSS point, corner" = fig_pal$series[1], 
  "GNSS point, other" = fig_pal$series[2],
  "Outlier, removed" = fig_pal$problem)
pt_type <- pt_type[names(pt_type) %in% pt_xy$type]

p_poly <- ggplot() +
  # Wider than the fitted polygon, so that it is visible where they overlap
  geom_polygon(data = localXY(corner_raw, "X", "Y", "plot_id", plot_origin),
    aes(x = X, y = Y, group = group), 
    fill = NA, colour = fig_pal$series[1], linewidth = 1.4) +
  geom_polygon(data = localXY(quad_xy, "X", "Y", "plot_id", plot_origin),
    aes(x = X, y = Y, group = group), 
    fill = NA, colour = fig_pal$line_muted, linewidth = 0.3) +
  geom_polygon(data = localXY(plot_xy, "X", "Y", "plot_id", plot_origin),
    aes(x = X, y = Y, group = group), 
    fill = NA, colour = fig_pal$ink, linewidth = 0.5) +
  geom_point(data = localXY(pt_xy, "X", "Y", "plot_id", plot_origin),
    aes(x = X, y = Y, colour = type, shape = type), size = 3.5, stroke = 1.2) +
  scale_colour_manual(NULL, values = pt_type, breaks = names(pt_type)) +
  scale_shape_manual(NULL, values = c("GNSS point, corner" = 16, 
    "GNSS point, other" = 16, "Outlier, removed" = 4)[names(pt_type)],
    breaks = names(pt_type)) +
  facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(nrow(plot_origin))) +
  coord_equal() +
  labs(
    title = "Plot polygons, quadrats and GNSS points",
    x = "Easting (m)",
    y = "Northing (m)") +
  themeMap()
saveFig(p_poly, file.path(report_dir, "07_plot_poly.png"), 
  n_panel = nrow(plot_origin))

# Figure of plot locations across the site, in longitude and latitude
corner_raw_poly <- st_sf(
  plot_id = unique(corner_raw$plot_id),
  geometry = st_sfc(lapply(split(corner_raw, corner_raw$plot_id)[unique(corner_raw$plot_id)],
    function(x) {
      st_polygon(list(rbind(cbind(x$X, x$Y), c(x$X[1], x$Y[1]))))
    }), crs = crs_site)) %>%
  st_transform(., 4326)
gnss_ll <- gnss_xy %>%
  st_as_sf(., coords = c("X", "Y"), crs = crs_site) %>%
  st_transform(., 4326) %>%
  bind_cols(., st_coordinates(.)) %>%
  st_drop_geometry()
p_location <- ggplot() +
  # Wider than the fitted polygon, so that it is visible where they overlap
  geom_sf(data = corner_raw_poly, aes(colour = "GNSS corners"), 
    fill = NA, linewidth = 1.6) +
  geom_point(data = gnss_ll, aes(x = X, y = Y, colour = "GNSS corners"),
    size = 0.8) +
  geom_sf(data = st_transform(plot_poly, 4326), aes(colour = "Fitted plot polygon"),
    fill = NA, linewidth = 0.5) +
  scale_colour_manual(NULL, values = c("GNSS corners" = fig_pal$series[1],
    "Fitted plot polygon" = fig_pal$ink)) +
  labs(
    title = "Plot locations",
    x = "Longitude",
    y = "Latitude") +
  themeMap()
saveFig(p_location, file.path(report_dir, "07_plot_location_site.png"), 
  width = 8, height = 7)

# Figure of stem locations within the quadrat grid, in plot coordinates
if (nrow(stem_sel) > 0) {
  quad_rel <- plot_divide$sub_corner_coord %>%
    group_by(plot_id = as.character(plot_ID), subplot_ID) %>%
    summarise(
      x_min = min(x_rel), x_max = max(x_rel),
      y_min = min(y_rel), y_max = max(y_rel),
      .groups = "drop")

  stem_xy <- stem_sel %>%
    mutate(plot_id = as.character(plot_id)) %>%
    left_join(., dplyr::select(stem_outside, record_id, action), 
      by = "record_id") %>%
    left_join(., dplyr::select(plot_divide$tree_data, record_id, grid_excl),
      by = "record_id") %>%
    mutate(type = case_when(
      grid_excl %in% TRUE ~ "In plot, left out of quadrat grid",
      is.na(action) ~ "Stem, in quadrat grid",
      action == "excluded" ~ "Outside grid, excluded",
      TRUE ~ "Outside grid, assigned to nearest quadrat"))

  stem_type <- c(
    "Stem, in quadrat grid" = fig_pal$series[1],
    "In plot, left out of quadrat grid" = fig_pal$line_muted,
    "Outside grid, assigned to nearest quadrat" = fig_pal$warn,
    "Outside grid, excluded" = fig_pal$problem)
  stem_type <- stem_type[names(stem_type) %in% stem_xy$type]
  stem_in_plot <- c("Stem, in quadrat grid", "In plot, left out of quadrat grid")
  n_plot_stem <- length(unique(stem_xy$plot_id))

  # Stems far outside the quadrat grid, e.g. with coordinate errors, are
  # shown at the edge of the map, so that plots are large enough to see
  map_x <- range(c(quad_rel$x_min, quad_rel$x_max, plot_ext$x_min, plot_ext$x_max))
  map_y <- range(c(quad_rel$y_min, quad_rel$y_max, plot_ext$y_min, plot_ext$y_max))
  map_margin <- 0.1 * max(diff(map_x), diff(map_y))
  map_x <- map_x + c(-1, 1) * map_margin
  map_y <- map_y + c(-1, 1) * map_margin
  stem_xy <- stem_xy %>%
    mutate(
      edge = ifelse(x_rel_m < map_x[1] | x_rel_m > map_x[2] | 
        y_rel_m < map_y[1] | y_rel_m > map_y[2], 
        "Beyond map edge, shown at edge", "Within map"),
      # Inset from the edge, so that points are not clipped
      x_rel_m = pmin(pmax(x_rel_m, map_x[1] + 0.25 * map_margin), 
        map_x[2] - 0.25 * map_margin),
      y_rel_m = pmin(pmax(y_rel_m, map_y[1] + 0.25 * map_margin), 
        map_y[2] - 0.25 * map_margin))

  p_stem <- ggplot() +
    geom_rect(data = filter(quad_rel, plot_id %in% stem_xy$plot_id),
      aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
      fill = NA, colour = fig_pal$line_muted, linewidth = 0.3) +
    geom_rect(data = filter(mutate(plot_ext, plot_id = as.character(plot_ID)), 
      plot_id %in% stem_xy$plot_id),
      aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
      fill = NA, colour = fig_pal$ink, linewidth = 0.4) +
    geom_point(data = filter(stem_xy, type %in% stem_in_plot),
      aes(x = x_rel_m, y = y_rel_m, colour = type), 
      size = 0.8, alpha = 0.7, shape = 16) +
    scale_colour_manual(NULL, values = stem_type, breaks = names(stem_type)) +
    guides(colour = guide_legend(order = 1,
      override.aes = list(size = 2, alpha = 1, shape = 16)))
  # Stems outside the plot, only if there are any, as an empty layer gives a
  # warning from the shape scale
  if (any(!stem_xy$type %in% stem_in_plot)) {
    p_stem <- p_stem +
      geom_point(data = filter(stem_xy, !type %in% stem_in_plot),
        aes(x = x_rel_m, y = y_rel_m, colour = type, shape = edge), size = 2.4) +
      scale_shape_manual(NULL, values = c("Within map" = 16,
        "Beyond map edge, shown at edge" = 17),
        breaks = intersect("Beyond map edge, shown at edge", stem_xy$edge)) +
      guides(shape = guide_legend(order = 2,
        override.aes = list(size = 2, colour = fig_pal$ink_muted)))
  }
  p_stem <- p_stem +
    facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_stem)) +
    coord_equal(xlim = map_x, ylim = map_y, expand = FALSE) +
    labs(
      title = "Stem locations and quadrats",
      x = "X (m)",
      y = "Y (m)") +
    themeMap()
  saveFig(p_stem, file.path(report_dir, "07_stem_map.png"), n_panel = n_plot_stem)
}
