# Test data outputs from all sites
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-24

# Packages
library(dplyr)
library(sf)

# Import quadrat summary objects
quad_summ_all <- bind_rows(lapply(list.files("./dat/sites/", "quad_summ.gpkg", 
    recursive = TRUE, full.names = TRUE), 
  st_read))

# Which quadrats contain no biomass?
quad_summ_all %>% 
  filter(agb_Mg_sum_mc_mean_ha == 0 | is.na(agb_Mg_sum_mc_mean_ha)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(site_id, plot_id, quadrat_id, census_id, agb_Mg_sum_mc_mean_ha)
