# Correct taxonomic names
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2026-02-11

# Combine taxa across tables
taxon_vec <- sort(unique(c(
  stem$taxon_name,
  wd$taxon_name)))

# Check names
taxa <- correctTaxo(
  genus = taxon_vec, 
  species = NULL,
  interactive = TRUE, 
  preferAccepted = TRUE,
  preferFuzzy = FALSE,
  sub_pattern = subPattern(),
  useCache = TRUE,
  useAPI = FALSE,
  capacity = 60,
  fill_time_s = 1, 
  timeout = 60)

# Write stem taxonomic information to file
write.csv(taxa, file.path(outdir, "taxa.csv"), row.names = FALSE)

# Write WFO cache to file
saveRDS(BIOMASS:::the$wfo_cache, file.path(outdir, "wfo_cache.rds"))



