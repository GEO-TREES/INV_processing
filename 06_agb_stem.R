# Estimate stem-level above-ground woody biomass from tree inventory data
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2026-02-16

# Estimate stem-level AGB
stem_agb <- stem %>% 
  left_join(., stem_wd, by = "record_id") %>% 
  left_join(., stem_height, by = "record_id") %>% 
  mutate(
    agb_Mg = computeAGB(
      D = .$diam_cm,
      WD = .$meanWD,
      H = .$height_m_pred),
    ba_m2 = base::pi * (.$diam_cm / 2)^2 / 10000) %>% 
  dplyr::select(
    record_id, 
    agb_Mg,
    ba_m2)

# Write stem-level AGB to file
write.csv(stem_agb, file.path(outdir, "stem_agb.csv"), row.names = FALSE)

