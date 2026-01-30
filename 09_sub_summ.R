# Summarise stem data within subplots
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(tidyr)
library(sf)
library(BIOMASS)
library(parallel)
library(units)

# Define directories
# outdir <- "./dat/sites/Panama Canal/09_sub_out"

# Import data
# stem_summ <- st_read("./dat/sites/Panama Canal/08_stem_summ/stem_summ.gpkg")
# sub_poly <- st_read("./dat/sites/Panama Canal/04_subplots/sub_poly.gpkg")

# Calculate area of each subplot
sub_poly_area <- st_drop_geometry(sub_poly)
sub_poly_area$area_ha <- st_area(sub_poly) * 0.0001

# Extract subplot centres
sub_cent <- st_centroid(sub_poly) %>% 
  st_transform(4326) %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(subplot_id, longitude = X, latitude = Y)

# Prepare stem dataframe
stem_summ_sub <- stem_summ %>% 
  st_drop_geometry() %>% 
  separate_longer_delim(subplot_id_vec, ";") %>% 
  rename(subplot_id = subplot_id_vec) %>% 
  filter(
    alive == 1,
    broken == 0, 
    fallen == 0,
    missing == 0,
    liana == 0,
    !is.na(subplot_id)) %>%
  left_join(., sub_cent, by = "subplot_id") %>% 
  left_join(., sub_poly_area, by = c("plot_id", "subplot_id"))

# Split stem dataframe (D >= 10 cm) by subplot
stem_summ_sub_ge10 <- stem_summ_sub %>% 
  filter(diam >= 10)
stem_summ_sub_ge10_split <- split(stem_summ_sub_ge10, stem_summ_sub_ge10$subplot_id)

# Define number of simulations
nsim <- 1000

# Run AGB MC error propagation
subs_length <- length(stem_summ_sub_ge10_split)
sub_agb_mc <- lapply(seq_along(stem_summ_sub_ge10_split), function(x) { 
  message(paste0(x, "/", subs_length, " - ", names(stem_summ_sub_ge10_split)[x]))
  if (nrow(stem_summ_sub_ge10_split[[x]]) < 2) {
    agb <- computeAGB(
      D = stem_summ_sub_ge10_split[[x]]$diam,
      WD = stem_summ_sub_ge10_split[[x]]$meanWD,
      coord = stem_summ_sub_ge10_split[[x]][,c("longitude", "latitude")])
    list(
      "meanAGB" = agb,
      "medAGB" = agb,
      "sdAGB" = NA_real_,
      "credibilityAGB" = c("2.5%" = NA_real_, "97.5%" = NA_real_),
      "AGB_simu" = NA_real_)
  } else {
    AGBmonteCarlo(
      D = stem_summ_sub_ge10_split[[x]]$diam,
      WD = stem_summ_sub_ge10_split[[x]]$meanWD,
      coord = stem_summ_sub_ge10_split[[x]][,c("longitude", "latitude")],
      Dpropag = "chave2004",
      errWD = stem_summ_sub_ge10_split[[x]]$sdWD,
      n = nsim)
  }
})
names(sub_agb_mc) <- names(stem_summ_sub_ge10_split)

# Extract summary statistics from AGB MC error propagation simulations
sub_agb_mc_summ <- bind_rows(lapply(names(sub_agb_mc), function(x) { 
  data.frame(
    subplot_id = x,
    agb_ge10_sum_mc_mean = sub_agb_mc[[x]]$meanAGB,
    agb_ge10_sum_mc_median = sub_agb_mc[[x]]$medAGB,
    agb_ge10_sum_mc_sd = sub_agb_mc[[x]]$sdAGB,
    agb_ge10_sum_mc_se = sub_agb_mc[[x]]$sdAGB / nsim,
    agb_ge10_sum_mc_ci2.5 = unname(sub_agb_mc[[x]]$credibilityAGB[1]),
    agb_ge10_sum_mc_ci97.5 = unname(sub_agb_mc[[x]]$credibilityAGB[2]))
}))

# Calculate simple AGB estimates
subs_summ <- stem_summ_sub %>% 
  group_by(site_id, plot_id, subplot_id, census_id, area_ha) %>% 
  summarise(
    ba_sum = sum(ba, na.rm = TRUE),
    ba_ge5_sum = sum(ba[diam >= 5], na.rm = TRUE),
    ba_ge10_sum = sum(ba[diam >= 10], na.rm = TRUE),
    ba_ge20_sum = sum(ba[diam >= 20], na.rm = TRUE),
    agb_sum = sum(agb, na.rm = TRUE),
    agb_ge5_sum = sum(agb[diam >= 5], na.rm = TRUE),
    agb_ge10_sum = sum(agb[diam >= 10], na.rm = TRUE),
    agb_ge20_sum = sum(agb[diam >= 20], na.rm = TRUE),
    diam_mean = mean(diam, na.rm = TRUE),
    diam_max = max(diam, na.rm = TRUE),
    diam_q80 = quantile(diam, 0.8, na.rm = TRUE),
    diam_q90 = quantile(diam, 0.9, na.rm = TRUE),
    diam_q95 = quantile(diam, 0.95, na.rm = TRUE),
    diam_ge5_mean = mean(diam[diam >= 5], na.rm = TRUE),
    diam_ge5_max = max(diam[diam >= 5], na.rm = TRUE),
    diam_ge5_q80 = quantile(diam[diam >= 5], 0.8, na.rm = TRUE),
    diam_ge5_q90 = quantile(diam[diam >= 5], 0.9, na.rm = TRUE),
    diam_ge5_q95 = quantile(diam[diam >= 5], 0.95, na.rm = TRUE),
    diam_ge10_mean = mean(diam[diam >= 10], na.rm = TRUE),
    diam_ge10_max = max(diam[diam >= 10], na.rm = TRUE),
    diam_ge10_q80 = quantile(diam[diam >= 10], 0.8, na.rm = TRUE),
    diam_ge10_q90 = quantile(diam[diam >= 10], 0.9, na.rm = TRUE),
    diam_ge10_q95 = quantile(diam[diam >= 10], 0.910, na.rm = TRUE),
    diam_ge20_mean = mean(diam[diam >= 20], na.rm = TRUE),
    diam_ge20_max = max(diam[diam >= 20], na.rm = TRUE),
    diam_ge20_q80 = quantile(diam[diam >= 20], 0.8, na.rm = TRUE),
    diam_ge20_q90 = quantile(diam[diam >= 20], 0.9, na.rm = TRUE),
    diam_ge20_q95 = quantile(diam[diam >= 20], 0.920, na.rm = TRUE),
    height_pred_mean = mean(height_pred, na.rm = TRUE),
    height_pred_max = max(height_pred, na.rm = TRUE),
    height_pred_q80 = quantile(height_pred, 0.8, na.rm = TRUE),
    height_pred_q90 = quantile(height_pred, 0.9, na.rm = TRUE),
    height_pred_q95 = quantile(height_pred, 0.95, na.rm = TRUE),
    height_pred_ge5_mean = mean(height_pred[height_pred >= 5], na.rm = TRUE),
    height_pred_ge5_max = max(height_pred[height_pred >= 5], na.rm = TRUE),
    height_pred_ge5_q80 = quantile(height_pred[height_pred >= 5], 0.8, na.rm = TRUE),
    height_pred_ge5_q90 = quantile(height_pred[height_pred >= 5], 0.9, na.rm = TRUE),
    height_pred_ge5_q95 = quantile(height_pred[height_pred >= 5], 0.95, na.rm = TRUE),
    height_pred_ge10_mean = mean(height_pred[height_pred >= 10], na.rm = TRUE),
    height_pred_ge10_max = max(height_pred[height_pred >= 10], na.rm = TRUE),
    height_pred_ge10_q80 = quantile(height_pred[height_pred >= 10], 0.8, na.rm = TRUE),
    height_pred_ge10_q90 = quantile(height_pred[height_pred >= 10], 0.9, na.rm = TRUE),
    height_pred_ge10_q95 = quantile(height_pred[height_pred >= 10], 0.910, na.rm = TRUE),
    height_pred_ge20_mean = mean(height_pred[height_pred >= 20], na.rm = TRUE),
    height_pred_ge20_max = max(height_pred[height_pred >= 20], na.rm = TRUE),
    height_pred_ge20_q80 = quantile(height_pred[height_pred >= 20], 0.8, na.rm = TRUE),
    height_pred_ge20_q90 = quantile(height_pred[height_pred >= 20], 0.9, na.rm = TRUE),
    height_pred_ge20_q95 = quantile(height_pred[height_pred >= 20], 0.920, na.rm = TRUE),
    lorey_height = sum(ba * height_pred, na.rm = TRUE) / sum(ba, na.rm = TRUE),
    meanWD_mean = mean(meanWD, na.rm = TRUE),
    meanWD_ge5_mean = mean(meanWD[diam >= 5], na.rm = TRUE),
    meanWD_ge10_mean = mean(meanWD[diam >= 10], na.rm = TRUE),
    meanWD_ge20_mean = mean(meanWD[diam >= 20], na.rm = TRUE)) %>% 
  left_join(., sub_agb_mc_summ, by = "subplot_id") %>% 
  mutate(
    across(starts_with(c("ba_", "agb_")), ~.x / area_ha, .names = "{.col}_ha"),
    across(
      .cols = where(~ inherits(.x, "units")), 
      .fns = drop_units),
    across(everything(), ~ifelse(.x == -Inf, NA_real_, .x)))

# Combine with polygons
polys_summ <- right_join(sub_poly, subs_summ, by = c("plot_id", "subplot_id"))

# Write to file
st_write(polys_summ, file.path(outdir, "sub_summ.gpkg"), delete_dsn = TRUE)

