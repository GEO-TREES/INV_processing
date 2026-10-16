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

  # Choose height diameter model, set by `height_model`. If "best", use the
  # model with the lowest residual standard error, calculated on the
  # untransformed height scale for all models, so comparable across log and
  # non-log models
  height_mod_best <- if (param$height_model == "best") {
    height_mod_comp$method[which.min(height_mod_comp$RSE)]
  } else {
    param$height_model
  }

  msgTable(height_mod_comp, paste0("Height-diameter models, fitted to ",
    nrow(height), " measurements. RSE in m. Model used: ", height_mod_best))

  # Fit best height diameter model
  height_mod <- modelHD(
    D = height$diam_cm,
    H = height$height_m,
    method = height_mod_best,
    bayesian = FALSE,
    useCache = FALSE,
    drawGraph = FALSE)

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

# Summarise predicted heights
height_check <- stem %>%
  dplyr::select(record_id, plot_id, diam_cm) %>%
  inner_join(., out, by = "record_id") %>%
  filter(!is.na(diam_cm), !is.na(height_m_pred))

height_summ <- height_check %>%
  summarise(
    n_stem = n(),
    height_m_min = min(height_m_pred),
    height_m_median = median(height_m_pred),
    height_m_max = max(height_m_pred),
    n_over_60m = sum(height_m_pred > 60))
msgTable(height_summ, paste0("Predicted stem height (", param$height_method, 
  " height-diameter model):"))

# Figure of predicted height against diameter, with field measurements and
# the fitted model if `height_method` is "field"
p_height <- ggplot() +
  geom_point(data = height_check, aes(x = diam_cm, y = height_m_pred, 
    colour = "Predicted, stems"), size = 0.6, alpha = 0.3, shape = 16)
if (param$height_method == "field") {
  height_obs <- filter(height, !is.na(diam_cm), !is.na(height_m))
  height_curve <- data.frame(diam_cm = seq(min(height_obs$diam_cm), 
    max(height_obs$diam_cm), length.out = 200))
  height_curve$height_m <- retrieveH(D = height_curve$diam_cm, model = height_mod)$H
  p_height <- p_height +
    geom_point(data = height_obs, aes(x = diam_cm, y = height_m, 
      colour = "Measured"), size = 1.2, alpha = 0.6, shape = 16) +
    geom_line(data = height_curve, aes(x = diam_cm, y = height_m, 
      colour = "Fitted model"), linewidth = 0.7)
}
p_height <- p_height +
  scale_colour_manual(NULL, 
    values = c("Predicted, stems" = fig_pal$series[1], 
      "Measured" = fig_pal$series[2], "Fitted model" = fig_pal$ink),
    breaks = c("Predicted, stems", "Measured", "Fitted model")) +
  guides(colour = if (param$height_method == "field") {
    guide_legend(override.aes = list(alpha = 1, size = 2))
  } else {
    "none"
  }) +
  labs(
    title = "Stem height against diameter",
    subtitle = paste0("Height method: ", param$height_method,
      if (param$height_method == "field") paste0(", model: ", height_mod_best) else 
        ", Chave et al. (2014) Eq. 6a at plot centres"),
    x = "Diameter (cm)",
    y = "Height (m)") +
  themeFig()
saveFig(p_height, file.path(checkdir, "04_height_diam.png"), width = 8, height = 6)
