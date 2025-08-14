# Summarise stem data within subplots
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(tidyr)
library(sf)

# Define directories
# outdir <- "./dat/sites/Panama Canal/09_sub_summ"

# Import data
# stems_all <- st_read("./dat/sites/Panama Canal/08_stem_out/stems_all.gpkg")
# polys_sub <- st_read("./dat/sites/Panama Canal/04_subplots/polys_sub.gpkg")

# Calculate area of each subplot
polys_sub_area <- st_drop_geometry(polys_sub)
polys_sub_area$area_ha <- units::drop_units(st_area(polys_sub)) * 0.0001

subs_summ <- stems_all %>% 
  st_drop_geometry() %>% 
  separate_longer_delim(subplot_id_vec, ";") %>% 
  rename(subplot_id = subplot_id_vec) %>% 
  left_join(., polys_sub_area, by = c("plot_id", "subplot_id")) %>% 
  filter(
    alive == 1,
    broken == 0, 
    fallen == 0,
    missing == 0,
    liana == 0,
    !is.na(subplot_id)) %>%
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
  mutate(across(starts_with(c("ba_", "agb_")), ~.x / area_ha, .names = "{.col}_ha")) 

# Combine with polygons
polys_summ <- right_join(polys_sub, subs_summ, by = c("plot_id", "subplot_id"))

# Write to file
st_write(polys_summ, file.path(outdir, "sub_summ.gpkg"), delete_dsn = TRUE)

