# Prepare L2 and L3 data products 
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-16

# Packages
library(dplyr)
library(sf)

source("./func.R")

# Define directories
# outdir <- "./dat/sites/Panama_Canal/12_brm"

# Import data
# stem_fil <- read.csv("./dat/sites/Panama_Canal/09_stem_fil/stem_fil.csv")
# stem_agb_mc <- read.csv("./dat/sites/Panama_Canal/10_agb_mc/stem_agb_mc.csv")
# quad_summ <- st_read("./dat/sites/Panama_Canal/11_quad_summ/quad_summ.gpkg")

# Prepare L2 dataset
L2 <- stem_fil %>% 
  group_by(quadrat_id) %>% 
  filter(census_id == max(census_id)) %>% 
  ungroup() %>% 
  left_join(., stem_agb_mc, by = "record_id") %>% 
  mutate(
    Tree_label = pasteVals(tree_id, stem_id, sep = ":"),
    agb_Mg_mean = ifelse(is.na(agb_Mg_mean), agb_Mg, agb_Mg_mean)) %>% 
  dplyr::select(
    BRM_site = site_id,
    Plot_name = plot_id,
    Tree_label,
    Longitude = longitude,
    Latitude = latitude,
    AGB_tree_estimate = agb_Mg_mean,
    AGB_tree_uncertainty = agb_Mg_sd,
    Height_tree_estimate = height_m_pred)#,
    # TODO: Height_tree_uncertainty = )

# Check all values filled
stopifnot(all(!is.na(L2$AGB_tree_estimate)))
# stopifnot(all(!is.na(L2$AGB_tree_uncertainty)))

# Prepare L3 dataset
L3 <- quad_summ %>% 
  group_by(quadrat_id) %>% 
  filter(census_id == max(census_id)) %>% 
  ungroup() %>% 
  dplyr::select(
    BRM_site = site_id,
    Plot_name = plot_id,
    Quadrat_name = quadrat_id,
    AGBD_stand_estimate = agb_Mg_sum_mc_mean_ha,
    AGBD_stand_uncertainty = agb_Mg_sum_mc_sd_ha,
    Height_stand_estimate = height_m_pred_max,
    # Height_stand_uncertainty = # TODO: Canopy height standard deviation, reporting the L2 uncertainty propagated to stand-level estimate. 
    Basal_area = ba_m2_sum_ha,
    Lorey_height = lorey_height_m,
    Wood_density = meanWD_wm_ba) 

# Check all values filled
stopifnot(all(!is.na(L3$AGBD_stand_estimate)))
# stopifnot(all(!is.na(L3$AGBD_stand_uncertainty)))

# Extract site ID
site_id <- unique(L3$BRM_site)

# Write L2 dataset to file
write.csv(L2, file.path(outdir, paste0(site_id, "_L2.csv")), row.names = FALSE)

# Write L3 dataset to file
st_write(L3, file.path(outdir, paste0(site_id, "_L3.gpkg")), delete_dsn = TRUE)

