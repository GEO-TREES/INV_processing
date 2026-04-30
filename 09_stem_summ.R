# Create a full stems table 
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-16

# Combine stem dataframes
stem_summ <- stem %>%
  left_join(., stem_agb, by = "record_id") %>% 
  left_join(., stem_agb_mc, by = "record_id") %>% 
  left_join(., stem_height, by = "record_id") %>% 
  left_join(., stem_wd, by = "record_id") %>% 
  left_join(., taxa, by = c("taxon_name" = "nameOriginal")) %>% 
  left_join(., stem_pt, by = "record_id") %>% 
  mutate(
    in_quadrat_calc = ifelse(record_id %in% record_fil, TRUE, FALSE)) %>% 
  # {
  #   if (exists("taxon")) {
  #     left_join(., taxon, by = "taxon_name")
  #   } else { . }
  # } %>% 
  st_sf()

# Write to file
st_write(stem_summ, file.path(outdir, "stem_summ.gpkg"), delete_dsn = TRUE) 

