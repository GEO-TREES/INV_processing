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
# outdir <- "./dat/sites/Panama_Canal/11_quad_summ"

# Import data
# stem_fil <- read.csv("./dat/sites/Panama_Canal/09_stem_fil/stem_fil.csv")
# quad_poly <- st_read("./dat/sites/Panama_Canal/04_quad/quad_poly.gpkg")
# quad_agb <- read.csv("./dat/sites/Panama_Canal/10_agb_mc/quad_agb.csv")
# plot_poly <- st_read("./dat/sites/Panama_Canal/01_plot/plot_poly.gpkg")

# Create quadrat polygon for each census ID

# Calculate area of each quadrat
quad_poly_area <- st_drop_geometry(quad_poly)
quad_poly_area$quadrat_area_ha <- drop_units(st_area(quad_poly)) * 0.0001

# Calculate quadrat summary values
quad_summ <- stem_fil %>% 
  st_drop_geometry() %>% 
  group_by(site_id, plot_id, quadrat_id, census_id) %>% 
  summarise(
    n_stem = n(),
    ba_m2_sum = sum(ba_m2, na.rm = TRUE),
    diam_cm_mean = mean(diam_cm, na.rm = TRUE),
    diam_cm_max = max(diam_cm, na.rm = TRUE),
    diam_cm_q80 = quantile(diam_cm, 0.8, na.rm = TRUE),
    diam_cm_q90 = quantile(diam_cm, 0.9, na.rm = TRUE),
    diam_cm_q95 = quantile(diam_cm, 0.95, na.rm = TRUE),
    height_m_pred_mean = mean(height_m_pred, na.rm = TRUE),
    height_m_pred_max = max(height_m_pred, na.rm = TRUE),
    height_m_pred_q80 = quantile(height_m_pred, 0.8, na.rm = TRUE),
    height_m_pred_q90 = quantile(height_m_pred, 0.9, na.rm = TRUE),
    height_m_pred_q95 = quantile(height_m_pred, 0.95, na.rm = TRUE),
    lorey_height_m = sum(ba_m2 * height_m_pred, na.rm = TRUE) / sum(ba_m2, na.rm = TRUE),
    meanWD_mean = mean(meanWD, na.rm = TRUE),
    meanWD_wm_ba = weighted.mean(meanWD, ba_m2)) %>% 
  ungroup() %>% 
  left_join(., quad_agb, by = c("quadrat_id", "census_id")) %>% 
  left_join(., quad_poly_area, by = c("plot_id", "quadrat_id")) %>% 
  mutate(
    across(
      starts_with(c("n_stem_", "ba_m2_", "agb_Mg")), 
      ~.x / quadrat_area_ha, .names = "{.col}_ha"),
    across(
      .cols = where(~inherits(.x, "units")), 
      .fns = drop_units),
    across(
      everything(), 
      ~ifelse(.x == -Inf, NA_real_, .x))) 

# Fill in quadrats which contain no stems 
quad_summ_out <- quad_poly %>% 
  left_join(., st_drop_geometry(plot_poly)[,c("plot_id", "census_id_all")], by = "plot_id") %>% 
  separate_longer_delim(census_id_all, ";") %>% 
  st_sf() %>% 
  mutate(census_id_all = as.integer(census_id_all)) %>% 
  rename(census_id = census_id_all) %>% 
  left_join(., quad_summ, by = c("plot_id", "quadrat_id", "census_id")) %>% 
  mutate(
    site_id = ifelse(is.na(site_id), unique(stem_fil$site_id), site_id),
    across(all_of(c(
      "n_stem", 
      "ba_m2_sum",
      "ba_m2_sum_ha",
      "agb_Mg_sum_mc_mean", 
      "agb_Mg_sum_mc_median",
      "agb_Mg_sum_mc_mean_ha", 
      "agb_Mg_sum_mc_median_ha")), ~ifelse(is.na(.x), 0, .x))) 

# Write to file
st_write(quad_summ_out, file.path(outdir, "quad_summ.gpkg"), delete_dsn = TRUE)

