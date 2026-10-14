# Summarise stem data within quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Ensure plot IDs are character class
quad_poly$plot_id <- as.character(quad_poly$plot_id)

# Calculate area of each quadrat
quad_poly_area <- st_drop_geometry(quad_poly)
quad_poly_area$quadrat_area_ha <- as.vector(st_area(quad_poly)) * 0.0001
quad_poly_area$quadrat_dim_x_m <- param$quad_dim[1]
quad_poly_area$quadrat_dim_y_m <- param$quad_dim[2]

# Calculate quadrat summary values
quad_summ_pre <- stem_summ %>% 
  filter(in_quadrat_calc == TRUE) %>% 
  st_drop_geometry() %>% 
  group_by(site_id, acquisition_id, plot_id, quadrat_id) %>% 
  summarise(
    census_date = median(measurement_date, na.rm = TRUE),
    census_date_res = case_when(
      any(measurement_date_res == "YYYY") ~ "YYYY",
      any(measurement_date_res == "YYYY-MM") ~ "YYYY-MM",
      any(measurement_date_res == "YYYY-MM-DD") ~ "YYYY-MM-DD",
      TRUE ~ NA_character_),
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
    lorey_height_m = sum(ba_m2 * height_m_pred, na.rm = TRUE) / 
      sum(ba_m2[!is.na(height_m_pred)], na.rm = TRUE),
    meanWD_mean = mean(meanWD, na.rm = TRUE),
    meanWD_wm_ba = weighted.mean(meanWD, ba_m2, na.rm = TRUE),
    .groups = "drop_last") %>% 
  ungroup() %>%
  mutate(
    across(
      .cols = where(~inherits(.x, "units")), 
      .fns = as.vector),
    # max() of only NAs returns -Inf
    across(
      .cols = where(is.numeric), 
      .fns = ~replace(.x, is.infinite(.x), NA_real_))) 
  
# add quadrat polygons, fill in quadrats with no trees
quad_summ <- quad_poly %>% 
  left_join(., quad_poly_area, 
    by = c("site_id", "acquisition_id", "plot_id", "quadrat_id")) %>% 
  left_join(., quad_summ_pre, 
    by = c("site_id", "acquisition_id", "plot_id", "quadrat_id")) %>% 
  # Quadrat AGBD from error propagation, including stems without coordinates
  left_join(., quad_agb, by = c("plot_id", "quadrat_id")) %>%
  mutate(
    empty_quadrat = is.na(n_stem),
    across(
      c(
        any_of(c(
          "n_stem",
          "ba_m2_sum",
          "volume_m3_sum",
          "agb_Mg_sum")),
        starts_with("agbd_Mg_ha_mc_")),
      ~if_else(empty_quadrat & is.na(.x), 0, .x)),
    across(
      c(
        all_of("n_stem"),
        starts_with(c("ba_m2_", "volume_m3_", "agb_Mg"))),
      ~.x / quadrat_area_ha,
      .names = "{.col}_ha")) %>% 
  dplyr::select(-empty_quadrat)

# Write to file
st_write(quad_summ, file.path(outdir, "quad_summ.gpkg"), delete_dsn = TRUE)

