# Summarise stem data within subplots
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(sf)

# Define directories
outdir <- "../../dat/sites/Panama Canal/08_sub_summ"

# Import data
stems <- read.csv("../../dat/sites/Panama Canal/02_stem_fmt/stems.csv")
biomass <- read.csv("../../dat/sites/Panama Canal/06_biomass/biomass.csv")
height <- read.csv("../../dat/sites/Panama Canal/05_height/height.csv")
wd <- read.csv("../../dat/sites/Panama Canal/04_wd/wd.csv")

pts_sub <- st_read("../../dat/sites/Panama Canal/07_subplots/pts_sub.gpkg")
polys_sub <- st_read("../../dat/sites/Panama Canal/07_subplots/polys_sub.gpkg")

# Combine stem dataframes
stems_all <- stems %>% 
  left_join(., biomass, by = "measurement_id") %>% 
  left_join(., height, by = "measurement_id") %>% 
  left_join(., wd, by = "measurement_id") 

# Calculate area of each subplot
polys_sub_area <- polys_sub %>% 
  mutate(area_ha = units::drop_units(st_area(polys_sub)) * 0.0001) %>% 
  dplyr::select(subplot_ID, area_ha) %>% 
  st_drop_geometry()

# Split subplot points by subplot ID
pts_sub_split <- split(pts_sub, pts_sub$subplot_ID)

# Assign stems to subplots
stems_subs <- bind_rows(lapply(pts_sub_split, function(dat) {
  out <- stems_all[
     stems_all$Plot_name == dat$corner_plot_ID[1] & 
       stems_all$x_rel <= max(dat$x_rel) & 
       stems_all$x_rel >= min(dat$x_rel) & 
       stems_all$y_rel <= max(dat$y_rel) & 
       stems_all$y_rel >= min(dat$y_rel),]
  out$subplot_ID <- dat$subplot_ID[rep(1, nrow(out))]
  out$area_ha <- polys_sub_area$area_ha[
    match(dat$subplot_ID[1], polys_sub_area$subplot_ID)][rep(1, nrow(out))]
  out
}))

subs_summ <- stems_subs %>% 
  group_by(subplot_ID, census_id, area_ha) %>% 
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

write.csv(subs_summ, file.path(outdir, "sub_summ.csv"), row.names = FALSE)
