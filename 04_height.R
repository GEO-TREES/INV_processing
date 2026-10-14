# Estimate stem height
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Regional height estimation
if (param$height_method == "regional") { 

  # Extract plot centres
  plot_pt_split <- plot_pt %>% 
    filter(!is.na(rover_easting_utm_m), !is.na(rover_northing_utm_m)) %>% 
    split(., .$crs_epsg)
  p_cent <- bind_rows(lapply(plot_pt_split, function(x) { 
    st_as_sf(x, coords = c("rover_easting_utm_m", "rover_northing_utm_m"), 
      crs = unique(x$crs_epsg)) %>% 
    st_transform(., 4326) 
  })) %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_centroid() %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(plot_id, X, Y)

  # Add plot centres to stem data
  s_cent <- stem %>% 
    left_join(., p_cent, by = "plot_id")

  stopifnot(all(!is.na(s_cent$X)))
  stopifnot(all(!is.na(s_cent$Y)))

  # Retrieve stem heights using plot locations
  height_pred <- retrieveH(
    D = s_cent$diam_cm, 
    coord = s_cent[,c("X", "Y")])

  # RSE of Chave et al. (2014) Eq. 6a, on the log scale
  # NOTE: not a standard deviation in metres, do not pass to AGBmonteCarlo(errH = )
  height_pred$RSE <- rep(0.243, length(height_pred$H))

  s_cent$height_m_pred <- height_pred$H
  s_cent$height_m_pred_rse <- height_pred$RSE

  # Create output dataframe
  out <- s_cent %>%
    dplyr::select(record_id, height_m_pred, height_m_pred_rse)

  # Write plot centres to file, used for AGB estimation
  write.csv(p_cent, file.path(outdir, "plot_cent.csv"), row.names = FALSE)
} else if (param$height_method == "field") { 

  # Compare height diameter models
  height_mod_comp <- modelHD(
    D = height$diam_cm,
    H = height$height_m,
    bayesian = FALSE,
    useCache = FALSE,
    drawGraph = FALSE)

  # Choose best height diameter model
  # Based on lowest residual standard error, calculated on the untransformed
  # height scale for all models, so comparable across log and non-log models
  height_mod_best <- height_mod_comp$method[which.min(height_mod_comp$RSE)]

  # Fit best height diameter model
  height_mod <- modelHD(
    D = height$diam_cm,
    H = height$height_m,
    method = height_mod_best,
    bayesian = FALSE,
    useCache = FALSE,
    drawGraph = TRUE)

  # Predict with best height diameter model
  height_est <- retrieveH(
    D = stem$diam_cm,
    model = height_mod)

  # Create output dataframe
  out <- stem %>% 
    mutate(
      height_m_pred = height_est$H,
      height_m_pred_rse = height_est$RSE) %>% 
    dplyr::select(record_id, height_m_pred, height_m_pred_rse)
}

# Write height estimates to file
write.csv(out, file.path(outdir, "stem_height.csv"), row.names = FALSE)

# Write height-diameter model to file
if (param$height_method == "field" && exists("height_mod")) {
  saveRDS(height_mod, file.path(outdir, "height_mod.rds"))
}
