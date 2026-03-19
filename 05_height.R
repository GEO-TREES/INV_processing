# Estimate stem height
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Regional height estimation
if (param$height_method == "regional") { 

  # Extract plot centres
  p_cent <- plot_pt %>% 
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
  s_cent$height_m_pred <- retrieveH(
    D = s_cent$diam_cm, 
    coord = s_cent[,c("X", "Y")])$H

  # Create output dataframe
  out <- s_cent %>% 
    dplyr::select(record_id, height_m_pred)
} else if (param$height_method == "field") { 

  # Compare height diameter models
  height_mod_comp <- modelHD(
    D = s_height$diam_cm,
    H = s_height$height_m,
    bayesian = TRUE,
    useCache = FALSE,
    drawGraph = FALSE)

  # Choose best height diameter model
  # Based on average bias
  height_mod_best <- height_mod_comp[
    height_mod_comp$Average_bias == min(height_mod_comp$Average_bias), "method"]

  # Fit best height diameter model
  height_mod <- modelHD(
    D = s_height$diam_cm,
    H = s_height$height_m,
    method = height_mod_best,
    bayesian = FALSE, # TODO: currently fails with TRUE
    useCache = FALSE,
    drawGraph = TRUE)

  # Predict with best height diameter model
  height_est <- retrieveH(
    D = stem$diam_cm,
    model = height_mod)

  # Create output dataframe
  out <- stem %>% 
    mutate(height_m_pred = height_est$H) %>% 
    dplyr::select(record_id, height_m_pred)
}

# Write to file
write.csv(out, file.path(outdir, "stem_height.csv"), row.names = FALSE)

## here we need to export/write height_mod
