# Estimate wood density for each measurement
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Join taxonomy data to stem data
stem_taxa_all <- left_join(stem, taxa, by = c("taxon_name" = "nameOriginal")

# Optionally process local wood density data
if (param$wd_method == "field") { 
  wd_summ <- wd %>% 
    left_join(., taxa, by = c("taxon_name" = "nameOriginal")) %>% 
  group_by(
    family = familyAccepted,
    genus = genusAccepted,
    species = speciesAccepted) %>% 
  summarise(
    meanWD = mean(wd, na.rm = TRUE),
    sdWD = sd(wd, na.rm = TRUE))
}

# Estimate wood density for each stem measurement
wd_all <- getWoodDensity(
  genus = stem_taxa_all$genusAccepted, 
  species = stem_taxa_all$speciesAccepted,
  stand = stem_taxa_all$plot_id,
  family = stem_taxa_all$familyAccepted,
  addWoodDensity = if (param$wd_method == "field") { wd_summ },
  verbose = TRUE)

# Create output dataframe
wd_out <- cbind(record_id = stem_taxa_all$record_id, wd_all)

# Write to file
write.csv(wd_out, file.path(outdir, "stem_wd.csv"), row.names = FALSE)
