# Test data outputs from all sites
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-24

# Packages
library(dplyr)
library(sf)

# Create dataframe of quad_summ files
quad_summ_files <- list.files("./dat/sites", "quad_summ.gpkg", 
  recursive = TRUE, full.names = TRUE)

quad_summ_files_split <- strsplit(quad_summ_files, "\\/")

quad_summ_files_df <- data.frame(
  filepath = quad_summ_files,
  site_id = unlist(lapply(quad_summ_files_split, "[[", 4)),
  acquisition_id = unlist(lapply(quad_summ_files_split, "[[", 6)),
  param_version = unlist(lapply(quad_summ_files_split, "[[", 7)),
  software_version = unlist(lapply(quad_summ_files_split, "[[", 8))
)

# Optionally filter to particular sites, versions, etc
quad_summ_files_df_fil <- quad_summ_files_df %>% 
  filter(site_id %in% c("Amacayacu", "Bicuar", "Misiones", "PanamaCanal"))

# Import quad_summ objects
# Add meta-data from filepath
quad_summ_list <- lapply(seq_len(nrow(quad_summ_files_df_fil)), function(x) { 
  out <- st_read(quad_summ_files_df_fil$filepath[x])
  out$param_version <- quad_summ_files_df_fil$param_version[x]
  out$software_version <- quad_summ_files_df_fil$software_version[x]
  return(out)
})

# Transform CRS to WGS84 and join into one dataframe
quad_summ_all <- bind_rows(lapply(quad_summ_list, st_transform, crs = 4326))

# Which quadrats within censuses contain no biomass?
quad_summ_all %>% 
  filter(agb_Mg_sum_mc_mean_ha == 0 | is.na(agb_Mg_sum_mc_mean_ha)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(site_id, acquisition_id, param_version, software_version, 
    plot_id, quadrat_id, agb_Mg_sum_mc_mean_ha) %>% 
  as_tibble() %>% 
  print(n = Inf)
