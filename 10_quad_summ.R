# Summarise stem data within quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Calculate area of each quadrat
quad_poly_area <- st_drop_geometry(quad_poly)
quad_poly_area$quadrat_area_ha <- as.vector(st_area(quad_poly)) * 0.0001
quad_poly_area$quadrat_dim_x_m <- param$quad_dim[1]
quad_poly_area$quadrat_dim_y_m <- param$quad_dim[2]

# Calculate quadrat summary values
quad_summ_pre <- stem_summ %>% 
  filter(in_quadrat_calc == TRUE) %>% 
  st_drop_geometry() %>% 
  group_by(site_id, acquisition_id, plot_id, quadrat_id, census_date) %>% 
  summarise(
    n_stem = n(),
    agb_Mg_sum = sum(agb_Mg, na.rm = TRUE),
    ba_m2_sum = sum(ba_m2, na.rm = TRUE),
    volume_m3_sum = sum(volume_m3, na.rm = TRUE),
    diam_cm_mean = mean(diam_cm, na.rm = TRUE),
    diam_cm_max = max(diam_cm, na.rm = TRUE),
    diam_cm_q80 = quantile(diam_cm, 0.8, na.rm = TRUE),
    diam_cm_q90 = quantile(diam_cm, 0.9, na.rm = TRUE),
    diam_cm_q95 = quantile(diam_cm, 0.95, na.rm = TRUE),
    height_m_pred_mean = mean(height_m_pred, na.rm = TRUE),
    height_m_pred_max_rse = height_m_pred_rse[which(height_m_pred == max(height_m_pred, na.rm = TRUE))][1],
    height_m_pred_max = max(height_m_pred, na.rm = TRUE),
    height_m_pred_q80 = quantile(height_m_pred, 0.8, na.rm = TRUE),
    height_m_pred_q90 = quantile(height_m_pred, 0.9, na.rm = TRUE),
    height_m_pred_q95 = quantile(height_m_pred, 0.95, na.rm = TRUE),
    lorey_height_m = sum(ba_m2 * height_m_pred, na.rm = TRUE) / sum(ba_m2, na.rm = TRUE),
    meanWD_mean = mean(meanWD, na.rm = TRUE),
    meanWD_wm_ba = weighted.mean(meanWD, ba_m2),
    .groups = "drop_last") %>% 
  ungroup() %>% 
  left_join(., quad_agb, by = "quadrat_id") %>% 
  left_join(., quad_poly_area, by = c("site_id", "acquisition_id", "plot_id", "quadrat_id")) %>% 
  mutate(
    across(
      starts_with(c("n_stem_", "ba_m2_", "volume_m3_", "agb_Mg")), 
      ~.x / quadrat_area_ha, .names = "{.col}_ha"),
    across(
      .cols = where(~inherits(.x, "units")), 
      .fns = as.vector),
    across(
      everything(), 
      ~ifelse(.x == -Inf, NA_real_, .x))) 

# Add quadrat polygons, fill in quadrats with no trees
quad_summ <- quad_poly %>% 
  left_join(., quad_summ_pre, 
    by = c("site_id", "acquisition_id", "plot_id", "quadrat_id")) %>% 
  mutate(
    across(all_of(c(
      "n_stem", 
      "ba_m2_sum",
      "ba_m2_sum_ha",
      "volume_m3_sum",
      "volume_m3_sum_ha",
      "agb_Mg_sum",
      "agb_Mg_sum_ha",
      "agb_Mg_sum_mc_mean", 
      "agb_Mg_sum_mc_mean_ha", 
      "agb_Mg_sum_mc_median",
      "agb_Mg_sum_mc_median_ha")), ~ifelse(is.na(.x), 0, .x))) 

# Write to file
st_write(quad_summ, file.path(outdir, "quad_summ.gpkg"), delete_dsn = TRUE)

