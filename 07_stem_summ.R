# Create a full stems table 
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-16

# Combine stem dataframes
stem_summ <- stem %>% 
  left_join(., stem_agb, by = "record_id") %>% 
  left_join(., stem_height, by = "record_id") %>% 
  left_join(., stem_wd, by = "record_id") %>% 
  left_join(., stem_taxa, by = "record_id") %>% 
  left_join(., stem_pt, by = "record_id") %>% 
  left_join(., plot[,c("site_id", "plot_id", "meas_diam_min_cm")], 
    by = c("site_id", "plot_id")) %>% 
  # {
  #   if (exists("taxon")) {
  #     left_join(., taxon, by = "taxon_name")
  #   } else { . }
  # } %>% 
  st_sf()

# Write to file
st_write(stem_summ, file.path(outdir, "stem_summ.gpkg"), delete_dsn = TRUE) 

