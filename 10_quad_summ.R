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


# Summarise quadrat AGB density, from error propagation (mc) and from the sum
# of stem AGB. They differ where trees without coordinates are assigned to
# quadrats at random, and because measurement errors are not symmetric on the
# AGB scale
quad_check <- st_drop_geometry(quad_summ) %>%
  mutate(
    cv_pct = 100 * agbd_Mg_ha_mc_sd / agbd_Mg_ha_mc_mean,
    diff_pct = 100 * (agbd_Mg_ha_mc_mean - agb_Mg_sum_ha) / agb_Mg_sum_ha)

quad_check_summ <- quad_check %>%
  summarise(
    n_quadrat = n(),
    n_empty = sum(n_stem == 0, na.rm = TRUE),
    area_ha_median = median(quadrat_area_ha),
    agbd_min = min(agbd_Mg_ha_mc_mean, na.rm = TRUE),
    agbd_median = median(agbd_Mg_ha_mc_mean, na.rm = TRUE),
    agbd_max = max(agbd_Mg_ha_mc_mean, na.rm = TRUE),
    cv_pct_median = median(cv_pct, na.rm = TRUE),
    n_diff_over_10pct = sum(abs(diff_pct) > 10, na.rm = TRUE))
msgTable(quad_check_summ, "Quadrat AGB density (Mg/ha, simulated mean):")

quad_diff <- quad_check %>%
  filter(abs(diff_pct) > 10) %>%
  arrange(desc(abs(diff_pct))) %>%
  transmute(plot_id, quadrat_id, n_stem, agbd_Mg_ha_mc_mean, agb_Mg_sum_ha,
    diff_pct = round(diff_pct, 1))
if (nrow(quad_diff) > 0) {
  msgTable(quad_diff, paste0("Quadrats where simulated AGB density differs ",
    "from the sum of stem AGB by more than 10%:"), max_rows = 10)
}

# Figure of quadrat AGB density in each plot. Coordinates are relative to
# the south-west corner of the bounding box of each plot
quad_xy <- polyCoords(quad_summ, "plot_id") %>%
  mutate(agbd = quad_check$agbd_Mg_ha_mc_mean[L2])
n_plot_quad <- length(unique(quad_xy$plot_id))
p_quad_map <- ggplot(localXY(quad_xy, "X", "Y", "plot_id"), 
  aes(x = X, y = Y, group = group, fill = agbd)) +
  geom_polygon(colour = fig_pal$surface, linewidth = 0.3) +
  scale_fill_gradientn("AGB density (Mg/ha)", colours = fig_pal$seq) +
  guides(fill = guide_colourbar(theme = theme(
    legend.title.position = "top",
    legend.key.width = unit(16, "lines"),
    legend.key.height = unit(0.8, "lines")))) +
  facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_quad)) +
  coord_equal() +
  labs(
    title = "Quadrat AGB density",
    subtitle = paste0("Simulated mean. Quadrats ", param$quad_dim[1], " x ",
      param$quad_dim[2], " m. Relative to the south-west of each plot"),
    x = "Easting (m)",
    y = "Northing (m)") +
  themeMap()
saveFig(p_quad_map, file.path(checkdir, "10_quad_agbd_map.png"), 
  n_panel = n_plot_quad)

# Figure comparing quadrat AGB density from error propagation and from the
# sum of stem AGB
quad_lim <- range(c(quad_check$agb_Mg_sum_ha, quad_check$agbd_Mg_ha_mc_ci2.5,
  quad_check$agbd_Mg_ha_mc_ci97.5), na.rm = TRUE)
p_quad_comp <- ggplot(quad_check, aes(x = agb_Mg_sum_ha, y = agbd_Mg_ha_mc_mean)) +
  geom_abline(intercept = 0, slope = 1, colour = fig_pal$ink_muted, 
    linewidth = 0.4) +
  geom_linerange(aes(ymin = agbd_Mg_ha_mc_ci2.5, ymax = agbd_Mg_ha_mc_ci97.5),
    colour = fig_pal$series[1], alpha = 0.25, linewidth = 0.4) +
  geom_point(colour = fig_pal$series[1], size = 1.2, alpha = 0.7, shape = 16) +
  coord_equal(xlim = quad_lim, ylim = quad_lim) +
  labs(
    title = "Quadrat AGB density: simulated against sum of stem AGB",
    subtitle = paste0(nrow(quad_check), " quadrats. Simulated mean with 95% ",
      "interval. Line shows 1:1"),
    x = "Sum of stem AGB (Mg/ha)",
    y = "Simulated AGB density (Mg/ha)") +
  themeFig()
saveFig(p_quad_comp, file.path(checkdir, "10_quad_agbd_compare.png"), 
  width = 7.5, height = 8)

# Quadrats with the most uncertain AGB density
quad_cv <- quad_check %>%
  filter(!is.na(cv_pct)) %>%
  arrange(desc(cv_pct)) %>%
  transmute(plot_id, quadrat_id, n_stem, agbd_Mg_ha_mc_mean, 
    agbd_Mg_ha_mc_ci2.5, agbd_Mg_ha_mc_ci97.5, cv_pct = round(cv_pct, 1))
msgTable(quad_cv, paste0("Quadrats with the most uncertain AGB density ",
  "(coefficient of variation, %):"), max_rows = 10)

# Figure of quadrat AGB density with 95% interval, quadrats ranked by AGB 
# density within each plot
quad_rank <- quad_check %>%
  filter(!is.na(agbd_Mg_ha_mc_mean)) %>%
  group_by(plot_id) %>%
  mutate(rank = rank(agbd_Mg_ha_mc_mean, ties.method = "first")) %>%
  ungroup()
n_plot_rank <- length(unique(quad_rank$plot_id))
p_quad_int <- ggplot(quad_rank, aes(x = rank)) +
  geom_linerange(aes(ymin = agbd_Mg_ha_mc_ci2.5, ymax = agbd_Mg_ha_mc_ci97.5),
    colour = fig_pal$seq[2], linewidth = 0.8) +
  geom_point(aes(y = agbd_Mg_ha_mc_mean), colour = fig_pal$series[1], 
    size = 1.4) +
  facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_rank), 
    scales = "free_x") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Quadrat AGB density with 95% interval",
    subtitle = paste0("Simulated mean and 95% interval. Quadrats ranked by AGB ",
      "density within each plot"),
    x = "Quadrats, ranked by AGB density",
    y = "AGB density (Mg/ha)") +
  themeFig() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank())
saveFig(p_quad_int, file.path(checkdir, "10_quad_agbd_interval.png"), 
  n_panel = n_plot_rank)

# Figure of the coefficient of variation of quadrat AGB density in each plot
quad_xy$cv <- quad_check$cv_pct[quad_xy$L2]
p_quad_cv_map <- ggplot(localXY(quad_xy, "X", "Y", "plot_id"), 
  aes(x = X, y = Y, group = group, fill = cv)) +
  geom_polygon(colour = fig_pal$surface, linewidth = 0.3) +
  scale_fill_gradientn("Coefficient of variation of AGB density (%)", 
    colours = fig_pal$seq2) +
  guides(fill = guide_colourbar(theme = theme(
    legend.title.position = "top",
    legend.key.width = unit(16, "lines"),
    legend.key.height = unit(0.8, "lines")))) +
  facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_quad)) +
  coord_equal() +
  labs(
    title = "Uncertainty in quadrat AGB density",
    subtitle = paste0("Coefficient of variation of simulations. Quadrats ", 
      param$quad_dim[1], " x ", param$quad_dim[2], 
      " m. Relative to the south-west of each plot"),
    x = "Easting (m)",
    y = "Northing (m)") +
  themeMap()
saveFig(p_quad_cv_map, file.path(checkdir, "10_quad_agbd_cv_map.png"), 
  n_panel = n_plot_quad)

# Figure of the coefficient of variation against quadrat AGB density
p_quad_cv <- ggplot(filter(quad_check, !is.na(cv_pct)), 
  aes(x = agbd_Mg_ha_mc_mean, y = cv_pct)) +
  geom_point(colour = fig_pal$series[2], size = 1.6, alpha = 0.7, shape = 16) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Uncertainty against quadrat AGB density",
    subtitle = paste0(sum(!is.na(quad_check$cv_pct)), " quadrats. ",
      "Coefficient of variation of simulations"),
    x = "Simulated AGB density (Mg/ha)",
    y = "Coefficient of variation (%)") +
  themeFig()
saveFig(p_quad_cv, file.path(checkdir, "10_quad_agbd_cv.png"), 
  width = 8, height = 6)
