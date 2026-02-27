# Estimate wood density for each measurement
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Join taxonomy data to stem data
stem_taxa_all <- left_join(stem, stem_taxa, by = "record_id")

# Estimate wood density for each stem measurement
wd_all <- getWoodDensity(
  genus = stem_taxa_all$genusAccepted, 
  species = stem_taxa_all$speciesAccepted,
  stand = stem_taxa_all$plot_id,
  family = stem_taxa_all$familyAccepted,
  addWoodDensity = NULL,
  verbose = TRUE)

wd_out <- cbind(record_id = stem_taxa$record_id, wd_all)

# Write to file
write.csv(wd_out, file.path(outdir, "stem_wd.csv"), row.names = FALSE)
