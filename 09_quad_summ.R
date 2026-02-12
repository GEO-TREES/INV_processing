# Summarise stem data within quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(tidyr)
library(sf)
library(BIOMASS)
library(units)

# Define directories
# outdir <- "./dat/sites/Panama Canal/09_quad_out"

# Import data
# stem_summ <- st_read("./dat/sites/Panama Canal/08_stem_summ/stem_summ.gpkg")
# quad_poly <- st_read("./dat/sites/Panama Canal/04_quad/quad_poly.gpkg")

# Calculate area of each quadrat
quad_poly_area <- st_drop_geometry(quad_poly)
quad_poly_area$area_ha <- st_area(quad_poly) * 0.0001

# Extract quadrat centres
quad_cent <- st_centroid(quad_poly) %>% 
  st_transform(4326) %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(quadrat_id, longitude = X, latitude = Y)

# Prepare stem dataframe
stem_summ_quad <- stem_summ %>% 
  st_drop_geometry() %>% 
  filter(
    !is.na(diam_cm),
    alive == TRUE,
    broken == FALSE, 
    fallen == FALSE,
    missing == FALSE,
    liana == FALSE,
    !is.na(quadrat_id)) %>%
  left_join(., quad_cent, by = "quadrat_id") %>% 
  left_join(., quad_poly_area, by = c("plot_id", "quadrat_id"))

# Split stem dataframe (D >= 10 cm) by quadrat
stem_summ_quad_ge10 <- stem_summ_quad[stem_summ_quad$diam_cm >= 10,]

stem_summ_quad_ge10_split <- split(stem_summ_quad_ge10, stem_summ_quad_ge10$quadrat_id)

# Define number of simulations
nsim <- 1000

# Run AGB MC error propagation
quad_length <- length(stem_summ_quad_ge10_split)
quad_agb_mc <- lapply(seq_along(stem_summ_quad_ge10_split), function(x) { 
  message(paste0(x, "/", quad_length, " - ", names(stem_summ_quad_ge10_split)[x]))
  if (nrow(stem_summ_quad_ge10_split[[x]]) < 2) {
    agb <- computeAGB(
      D = stem_summ_quad_ge10_split[[x]]$diam_cm,
      WD = stem_summ_quad_ge10_split[[x]]$meanWD,
      coord = stem_summ_quad_ge10_split[[x]][,c("longitude", "latitude")])
    list(
      "meanAGB" = agb,
      "medAGB" = agb,
      "sdAGB" = NA_real_,
      "credibilityAGB" = c("2.5%" = NA_real_, "97.5%" = NA_real_),
      "AGB_simu" = NA_real_)
  } else {
    AGBmonteCarlo(
      D = stem_summ_quad_ge10_split[[x]]$diam_cm,
      WD = stem_summ_quad_ge10_split[[x]]$meanWD,
      coord = stem_summ_quad_ge10_split[[x]][,c("longitude", "latitude")],
      Dpropag = "chave2004",
      errWD = stem_summ_quad_ge10_split[[x]]$sdWD,
      n = nsim)
  }
})
names(quad_agb_mc) <- names(stem_summ_quad_ge10_split)

# Extract summary statistics from AGB MC error propagation simulations
quad_agb_mc_summ <- bind_rows(lapply(names(quad_agb_mc), function(x) { 
  data.frame(
    quadrat_id = x,
    agb_ge10_sum_mc_mean = quad_agb_mc[[x]]$meanAGB,
    agb_ge10_sum_mc_median = quad_agb_mc[[x]]$medAGB,
    agb_ge10_sum_mc_sd = quad_agb_mc[[x]]$sdAGB,
    agb_ge10_sum_mc_se = quad_agb_mc[[x]]$sdAGB / nsim,
    agb_ge10_sum_mc_ci2.5 = unname(quad_agb_mc[[x]]$credibilityAGB[1]),
    agb_ge10_sum_mc_ci97.5 = unname(quad_agb_mc[[x]]$credibilityAGB[2]))
}))

# Calculate simple AGB estimates
quad_summ <- stem_summ_quad %>% 
  group_by(site_id, plot_id, quadrat_id, census_id, area_ha) %>% 
  summarise(
    ba_m2_sum = sum(ba_m2, na.rm = TRUE),
    ba_m2_ge5_sum = sum(ba_m2[diam_cm >= 5], na.rm = TRUE),
    ba_m2_ge10_sum = sum(ba_m2[diam_cm >= 10], na.rm = TRUE),
    ba_m2_ge20_sum = sum(ba_m2[diam_cm >= 20], na.rm = TRUE),
    agb_Mg_sum = sum(agb_Mg, na.rm = TRUE),
    agb_Mg_ge5_sum = sum(agb_Mg[diam_cm >= 5], na.rm = TRUE),
    agb_Mg_ge10_sum = sum(agb_Mg[diam_cm >= 10], na.rm = TRUE),
    agb_Mg_ge20_sum = sum(agb_Mg[diam_cm >= 20], na.rm = TRUE),
    diam_cm_mean = mean(diam_cm, na.rm = TRUE),
    diam_cm_max = max(diam_cm, na.rm = TRUE),
    diam_cm_q80 = quantile(diam_cm, 0.8, na.rm = TRUE),
    diam_cm_q90 = quantile(diam_cm, 0.9, na.rm = TRUE),
    diam_cm_q95 = quantile(diam_cm, 0.95, na.rm = TRUE),
    diam_cm_ge5_mean = mean(diam_cm[diam_cm >= 5], na.rm = TRUE),
    diam_cm_ge5_max = max(diam_cm[diam_cm >= 5], na.rm = TRUE),
    diam_cm_ge5_q80 = quantile(diam_cm[diam_cm >= 5], 0.8, na.rm = TRUE),
    diam_cm_ge5_q90 = quantile(diam_cm[diam_cm >= 5], 0.9, na.rm = TRUE),
    diam_cm_ge5_q95 = quantile(diam_cm[diam_cm >= 5], 0.95, na.rm = TRUE),
    diam_cm_ge10_mean = mean(diam_cm[diam_cm >= 10], na.rm = TRUE),
    diam_cm_ge10_max = max(diam_cm[diam_cm >= 10], na.rm = TRUE),
    diam_cm_ge10_q80 = quantile(diam_cm[diam_cm >= 10], 0.8, na.rm = TRUE),
    diam_cm_ge10_q90 = quantile(diam_cm[diam_cm >= 10], 0.9, na.rm = TRUE),
    diam_cm_ge10_q95 = quantile(diam_cm[diam_cm >= 10], 0.910, na.rm = TRUE),
    diam_cm_ge20_mean = mean(diam_cm[diam_cm >= 20], na.rm = TRUE),
    diam_cm_ge20_max = max(diam_cm[diam_cm >= 20], na.rm = TRUE),
    diam_cm_ge20_q80 = quantile(diam_cm[diam_cm >= 20], 0.8, na.rm = TRUE),
    diam_cm_ge20_q90 = quantile(diam_cm[diam_cm >= 20], 0.9, na.rm = TRUE),
    diam_cm_ge20_q95 = quantile(diam_cm[diam_cm >= 20], 0.920, na.rm = TRUE),
    height_m_pred_mean = mean(height_m_pred, na.rm = TRUE),
    height_m_pred_max = max(height_m_pred, na.rm = TRUE),
    height_m_pred_q80 = quantile(height_m_pred, 0.8, na.rm = TRUE),
    height_m_pred_q90 = quantile(height_m_pred, 0.9, na.rm = TRUE),
    height_m_pred_q95 = quantile(height_m_pred, 0.95, na.rm = TRUE),
    height_m_pred_ge5_mean = mean(height_m_pred[height_m_pred >= 5], na.rm = TRUE),
    height_m_pred_ge5_max = max(height_m_pred[height_m_pred >= 5], na.rm = TRUE),
    height_m_pred_ge5_q80 = quantile(height_m_pred[height_m_pred >= 5], 0.8, na.rm = TRUE),
    height_m_pred_ge5_q90 = quantile(height_m_pred[height_m_pred >= 5], 0.9, na.rm = TRUE),
    height_m_pred_ge5_q95 = quantile(height_m_pred[height_m_pred >= 5], 0.95, na.rm = TRUE),
    height_m_pred_ge10_mean = mean(height_m_pred[height_m_pred >= 10], na.rm = TRUE),
    height_m_pred_ge10_max = max(height_m_pred[height_m_pred >= 10], na.rm = TRUE),
    height_m_pred_ge10_q80 = quantile(height_m_pred[height_m_pred >= 10], 0.8, na.rm = TRUE),
    height_m_pred_ge10_q90 = quantile(height_m_pred[height_m_pred >= 10], 0.9, na.rm = TRUE),
    height_m_pred_ge10_q95 = quantile(height_m_pred[height_m_pred >= 10], 0.910, na.rm = TRUE),
    height_m_pred_ge20_mean = mean(height_m_pred[height_m_pred >= 20], na.rm = TRUE),
    height_m_pred_ge20_max = max(height_m_pred[height_m_pred >= 20], na.rm = TRUE),
    height_m_pred_ge20_q80 = quantile(height_m_pred[height_m_pred >= 20], 0.8, na.rm = TRUE),
    height_m_pred_ge20_q90 = quantile(height_m_pred[height_m_pred >= 20], 0.9, na.rm = TRUE),
    height_m_pred_ge20_q95 = quantile(height_m_pred[height_m_pred >= 20], 0.920, na.rm = TRUE),
    lorey_height = sum(ba_m2 * height_m_pred, na.rm = TRUE) / sum(ba_m2, na.rm = TRUE),
    meanWD_mean = mean(meanWD, na.rm = TRUE),
    meanWD_ge5_mean = mean(meanWD[diam_cm >= 5], na.rm = TRUE),
    meanWD_ge10_mean = mean(meanWD[diam_cm >= 10], na.rm = TRUE),
    meanWD_ge20_mean = mean(meanWD[diam_cm >= 20], na.rm = TRUE)) %>% 
  left_join(., quad_agb_mc_summ, by = "quadrat_id") %>% 
  mutate(
    across(starts_with(c("ba_m2_", "agb_Mg")), ~.x / area_ha, .names = "{.col}_ha"),
    across(
      .cols = where(~ inherits(.x, "units")), 
      .fns = drop_units),
    across(everything(), ~ifelse(.x == -Inf, NA_real_, .x)))

# Combine with polygons
polys_summ <- right_join(quad_poly, quad_summ, by = c("plot_id", "quadrat_id"))

# Write to file
st_write(polys_summ, file.path(outdir, "quad_summ.gpkg"), delete_dsn = TRUE)

