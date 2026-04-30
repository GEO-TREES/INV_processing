# Estimate wood density for each measurement
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Join taxonomy data to stem data
stem_taxa_all <- left_join(stem, taxa, by = c("taxon_name" = "nameOriginal"))

# Optionally process local wood density data
if (param$wd_method == "field") { 
  wd_summ <- wd %>%
    mutate(wood_density_n = ifelse(is.na(wood_density_n), 1, wood_density_n)) %>% 
    group_by(site_id, taxon_name) %>%
    summarise(
      wood_density_n_total = sum(wood_density_n, na.rm = TRUE),
      meanWD = sum(wood_density_gcm3 * wood_density_n, na.rm = TRUE) / 
        sum(wood_density_n[!is.na(wood_density_gcm3)], na.rm = TRUE),
      sdWD = {
        # Filter to rows that have both mean and n 
        valid_rows = !is.na(wood_density_gcm3) & !is.na(wood_density_n)
        
        # if SD is NA, treat as 0 (no variance) 
        # but still account distance from grand mean
        clean_sd = ifelse(is.na(wood_density_sd_gcm3), 0, wood_density_sd_gcm3)
        
        # Components for grand variance
        ss_within  = sum((wood_density_n[valid_rows] - 1) * 
          clean_sd[valid_rows]^2, na.rm = TRUE)
        ss_between = sum(wood_density_n[valid_rows] * 
          (wood_density_gcm3[valid_rows] - meanWD)^2, na.rm = TRUE)
        
        # If wood_density_n_total <= 1, return NA; otherwise calculate SD
        if (wood_density_n_total > 1) {
          sqrt((ss_within + ss_between) / (wood_density_n_total - 1)) 
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    ) %>% 
    left_join(., taxa, by = c("taxon_name" = "nameOriginal")) %>% 
    dplyr::select(
      family = familyAccepted,
      genus = genusAccepted,
      species = speciesAccepted,
      meanWD,
      sdWD)
}

# Estimate wood density for each stem measurement
wd_all <- BIOMASS::getWoodDensity(
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
