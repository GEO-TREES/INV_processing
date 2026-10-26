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
        starts_with(c("agbd_Mg_ha_mc_", "agbd_Mg_ha_crown_mc_"))),
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

# Colour scale of quadrat AGB density, the same in each figure. Histogram 
# bins can be centred slightly outside the range of the data, so values 
# outside the limits take the colour of the nearest limit
agbd_lim <- range(quad_check$agbd_Mg_ha_mc_mean, na.rm = TRUE)
agbd_fill <- scale_fill_gradientn("AGBD (Mg/ha)", colours = fig_pal$seq,
  limits = agbd_lim, oob = scales::squish, guide = colourBar())
agbd_colour <- scale_colour_gradientn("AGBD (Mg/ha)", colours = fig_pal$seq,
  limits = agbd_lim, oob = scales::squish, guide = colourBar())

# Figure of quadrat AGB density in each plot. Coordinates are relative to
# the south-west corner of the bounding box of each plot
quad_xy <- polyCoords(quad_summ, "plot_id") %>%
  mutate(agbd = quad_check$agbd_Mg_ha_mc_mean[L2])
n_plot_quad <- length(unique(quad_xy$plot_id))
quad_xy_local <- localXY(quad_xy, "X", "Y", "plot_id")
p_quad_map <- ggplot(quad_xy_local,
  aes(x = X, y = Y, group = group, fill = agbd)) +
  geom_blank(data = squareExtent(quad_xy_local, "X", "Y", "plot_id"),
    aes(x = X, y = Y), inherit.aes = FALSE) +
  geom_polygon(colour = fig_pal$surface, linewidth = 0.3) +
  agbd_fill +
  facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_quad),
    scales = "free") +
  labs(
    title = "Quadrat AGBD",
    x = "Easting (m)",
    y = "Northing (m)") +
  themeMap() +
  theme(aspect.ratio = 1)
saveFig(p_quad_map, file.path(report_dir, "10_quad_agbd_map.png"), 
  n_panel = n_plot_quad)

# Figure comparing quadrat AGB density from error propagation and from the
# sum of stem AGB
quad_lim <- range(c(quad_check$agb_Mg_sum_ha, quad_check$agbd_Mg_ha_mc_ci2.5,
  quad_check$agbd_Mg_ha_mc_ci97.5), na.rm = TRUE)
# Quadrats where the 95% interval is entirely above the 1:1 line, e.g. with
# trees without coordinates assigned to them at random, are labelled
quad_above <- filter(quad_check, agbd_Mg_ha_mc_ci2.5 > agb_Mg_sum_ha)
p_quad_comp <- ggplot(quad_check, aes(x = agb_Mg_sum_ha, y = agbd_Mg_ha_mc_mean)) +
  geom_abline(intercept = 0, slope = 1, colour = fig_pal$ink_muted, 
    linewidth = 0.5, linetype = 2) +
  geom_linerange(aes(ymin = agbd_Mg_ha_mc_ci2.5, ymax = agbd_Mg_ha_mc_ci97.5),
    colour = fig_pal$series[1], alpha = 0.25, linewidth = 0.4) +
  geom_point(colour = fig_pal$series[1], size = 1.2, alpha = 0.7, shape = 16) +
  # Labels are moved apart so that they do not overlap
  ggrepel::geom_text_repel(data = quad_above, aes(label = quadrat_id), 
    size = 2.8, colour = fig_pal$ink_muted, segment.colour = fig_pal$line_muted,
    min.segment.length = 0, max.overlaps = Inf, seed = 42) +
  coord_equal(xlim = quad_lim, ylim = quad_lim) +
  labs(
    title = "Quadrat AGBD: simulated against sum of stem AGB",
    subtitle = paste0(nrow(quad_check), " quadrats. Simulated mean with 95% ",
      "interval. Dashed line shows 1:1. Quadrats where the 95% interval is ",
      "above the 1:1 line are labelled"),
    x = "Sum of stem AGB (Mg/ha)",
    y = "Simulated AGBD (Mg/ha)") +
  themeFig()
saveFig(p_quad_comp, file.path(report_dir, "10_quad_agbd_compare.png"), 
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
# Lines and points are scaled to the width available for each quadrat in the
# panel of the plot with the most quadrats, up to a maximum size for plots
# with few quadrats
quad_slot_mm <- 25.4 * (figWidth(n_plot_rank) - 1.4) / figCols(n_plot_rank) /
  max(table(quad_rank$plot_id))
p_quad_int <- ggplot(quad_rank, aes(x = rank)) +
  geom_linerange(aes(ymin = agbd_Mg_ha_mc_ci2.5, ymax = agbd_Mg_ha_mc_ci97.5),
    colour = fig_pal$seq[3], linewidth = min(2.2, max(0.2, 0.1 * quad_slot_mm))) +
  geom_point(aes(y = agbd_Mg_ha_mc_mean), colour = fig_pal$seq[6],
    size = min(4.5, max(0.3, 0.2 * quad_slot_mm))) +
  facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_rank), 
    scales = "free_x") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Quadrat AGBD with 95% interval",
    subtitle = paste0("Simulated mean and 95% interval. Quadrats ranked by AGBD ",
      "within each plot"),
    x = "Quadrats, ranked by AGBD",
    y = "AGBD (Mg/ha)") +
  themeFig() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank())
saveFig(p_quad_int, file.path(report_dir, "10_quad_agbd_interval.png"), 
  n_panel = n_plot_rank)

# Figure of the coefficient of variation of quadrat AGB density in each plot
quad_xy_local$cv <- quad_check$cv_pct[quad_xy_local$L2]
p_quad_cv_map <- ggplot(quad_xy_local,
  aes(x = X, y = Y, group = group, fill = cv)) +
  geom_blank(data = squareExtent(quad_xy_local, "X", "Y", "plot_id"),
    aes(x = X, y = Y), inherit.aes = FALSE) +
  geom_polygon(colour = fig_pal$surface, linewidth = 0.3) +
  scale_fill_gradientn("Coefficient of variation of AGBD (%)", 
    colours = fig_pal$seq2, guide = colourBar()) +
  facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_quad),
    scales = "free") +
  labs(
    title = "Uncertainty in quadrat AGBD",
    x = "Easting (m)",
    y = "Northing (m)") +
  themeMap() +
  theme(aspect.ratio = 1)
saveFig(p_quad_cv_map, file.path(report_dir, "10_quad_agbd_cv_map.png"), 
  n_panel = n_plot_quad)

# Figure of the coefficient of variation against quadrat AGB density
p_quad_cv <- ggplot(filter(quad_check, !is.na(cv_pct)), 
  aes(x = agbd_Mg_ha_mc_mean, y = cv_pct)) +
  geom_point(colour = fig_pal$series[2], size = 1.6, alpha = 0.7, shape = 16) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Uncertainty against quadrat AGBD",
    x = "Simulated AGBD (Mg/ha)",
    y = "Coefficient of variation (%)") +
  themeFig()
saveFig(p_quad_cv, file.path(report_dir, "10_quad_agbd_cv.png"), 
  width = 8, height = 6)

# Figure of the distribution of quadrat AGB density, all plots
p_quad_hist <- ggplot(quad_check, aes(x = agbd_Mg_ha_mc_mean)) +
  geom_histogram(aes(fill = after_stat(x)), bins = 25,
    colour = fig_pal$surface, linewidth = 0.2) +
  agbd_fill +
  guides(fill = "none") +
  labs(
    title = "Quadrat AGBD",
    x = "Quadrat AGBD (Mg/ha)", 
    y = "Quadrats") +
  themeFig()
saveFig(p_quad_hist, file.path(report_dir, "10_quad_agbd_hist.png"), 
  width = 8, height = 5)

# Figure of quadrat AGB density across the site, in longitude and latitude
quad_ll <- st_transform(quad_summ, 4326)
p_quad_map_site <- ggplot() +
  geom_sf(data = quad_ll, aes(fill = agbd_Mg_ha_mc_mean),
    colour = fig_pal$surface, linewidth = 0.1) +
  agbd_fill +
  labs(
    title = "Quadrat AGBD",
    x = "Longitude", 
    y = "Latitude") +
  themeMap() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
saveFig(p_quad_map_site, file.path(report_dir, "10_quad_agbd_map_site.png"), 
  width = 8, height = 8)

# Figure of quadrat AGB density with its 95% interval, all quadrats of the
# site ranked by AGB density. Means are joined by a black line, so they stay
# visible when intervals of many quadrats overlap. Points are added for few
# quadrats
quad_rank <- quad_check %>%
  arrange(agbd_Mg_ha_mc_mean) %>%
  mutate(rank = row_number())
p_quad_int_site <- ggplot(quad_rank, aes(y = rank)) +
  geom_linerange(aes(xmin = agbd_Mg_ha_mc_ci2.5, xmax = agbd_Mg_ha_mc_ci97.5,
    colour = agbd_Mg_ha_mc_mean), linewidth = 0.4) +
  geom_path(aes(x = agbd_Mg_ha_mc_mean), colour = fig_pal$ink, linewidth = 0.6)
if (nrow(quad_rank) <= 100) {
  p_quad_int_site <- p_quad_int_site +
    geom_point(aes(x = agbd_Mg_ha_mc_mean), colour = fig_pal$ink, size = 0.9)
}
p_quad_int_site <- p_quad_int_site +
  agbd_colour +
  guides(colour = "none") +
  labs(
    title = "Quadrat AGBD and its uncertainty",
    subtitle = paste0("Simulated mean (black line) and 95% interval, all ",
      "quadrats ranked by AGBD"),
    x = "Quadrat AGBD (Mg/ha)", 
    y = "Quadrat rank") +
  themeFig()
saveFig(p_quad_int_site, file.path(report_dir, "10_quad_agbd_interval_site.png"), 
  width = 8, height = 6)

# Figures of stem AGB, for stems used to estimate AGB density, across the
# site in longitude and latitude, and in each plot, relative to the 
# south-west of the quadrat grid of each plot as in the map of quadrat AGB
# density
stem_map <- stem_summ %>%
  filter(in_quadrat_calc %in% TRUE, !st_is_empty(.), !is.na(agb_Mg), agb_Mg > 0) %>%
  mutate(plot_id = as.character(plot_id)) %>%
  arrange(agb_Mg)
# The lightest colour of the ramp is not used, so that stems with little AGB
# stay visible
stem_agb_colour <- scale_colour_gradientn("Stem AGB (Mg)", colours = fig_pal$seq2[-1],
  trans = "log10", breaks = 10^(-3:3),
  labels = function(x) format(x, drop0trailing = TRUE, scientific = FALSE, trim = TRUE),
  guide = colourBar())

p_stem_map_site <- ggplot() +
  geom_sf(data = quad_ll, fill = NA, colour = fig_pal$line_muted, linewidth = 0.2) +
  geom_sf(data = st_transform(stem_map, 4326), aes(colour = agb_Mg), 
    size = 0.8, shape = 16) +
  stem_agb_colour +
  labs(
    title = "Stem AGB",
    x = "Longitude", 
    y = "Latitude") +
  themeMap() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
saveFig(p_stem_map_site, file.path(report_dir, "10_stem_agb_map_site.png"), 
  width = 8, height = 8)

quad_xy0 <- quad_xy %>%
  group_by(plot_id) %>%
  summarise(x0 = min(X), y0 = min(Y), .groups = "drop")
stem_map_xy <- stem_map %>%
  st_drop_geometry() %>%
  filter(plot_id %in% quad_xy0$plot_id) %>%
  localXY(., "x_utm", "y_utm", "plot_id", quad_xy0)
p_stem_map <- ggplot() +
  geom_blank(data = squareExtent(quad_xy_local, "X", "Y", "plot_id"),
    aes(x = X, y = Y)) +
  geom_polygon(data = quad_xy_local,
    aes(x = X, y = Y, group = group),
    fill = NA, colour = fig_pal$line_muted, linewidth = 0.3) +
  geom_point(data = stem_map_xy, aes(x = x_utm, y = y_utm, colour = agb_Mg),
    size = 1.2, shape = 16) +
  stem_agb_colour +
  facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_quad),
    scales = "free") +
  labs(
    title = "Stem AGB in each plot",
    subtitle = paste0("Stems used to estimate AGBD, with ", 
      param$quad_dim[1], " x ", param$quad_dim[2], " m quadrats. Relative to ",
      "the south-west of each plot"),
    x = "Easting (m)",
    y = "Northing (m)") +
  themeMap() +
  theme(aspect.ratio = 1)
saveFig(p_stem_map, file.path(report_dir, "10_stem_agb_map.png"), 
  n_panel = n_plot_quad)
