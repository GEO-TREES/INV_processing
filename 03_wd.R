# Estimate wood density for each measurement
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-21

# Join taxonomy data to stem data
stem_taxa_all <- left_join(stem, taxa, by = c("taxon_name" = "nameOriginal"))

# Optionally process local wood density data
if (param$wd_method == "field") { 
  # Summarise by accepted name, so that original names which resolve to the 
  # same taxon are pooled
  wd_summ <- wd %>%
    left_join(., taxa, by = c("taxon_name" = "nameOriginal")) %>% 
    filter(!is.na(genusAccepted), !is.na(wood_density_gcm3)) %>% 
    mutate(wood_density_n = ifelse(is.na(wood_density_n), 1, wood_density_n)) %>% 
    group_by(
      family = familyAccepted, 
      genus = genusAccepted, 
      species = speciesAccepted) %>%
    summarise(
      wood_density_n_total = sum(wood_density_n),
      meanWD = sum(wood_density_gcm3 * wood_density_n) / wood_density_n_total,
      sdWD = {
        # If SD is NA, treat as 0 (no variance) 
        # but still account distance from grand mean
        clean_sd = ifelse(is.na(wood_density_sd_gcm3), 0, wood_density_sd_gcm3)
        
        # Components for grand variance
        ss_within  = sum((wood_density_n - 1) * clean_sd^2)
        ss_between = sum(wood_density_n * (wood_density_gcm3 - meanWD)^2)
        
        # If wood_density_n_total <= 1, return NA; otherwise calculate SD
        if (wood_density_n_total > 1) {
          sqrt((ss_within + ss_between) / (wood_density_n_total - 1)) 
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    ) %>% 
    dplyr::select(family, genus, species, meanWD, sdWD)
}

# Estimate wood density for each stem measurement
wd_all <- getWoodDensity(
  genus = stem_taxa_all$genusAccepted, 
  species = stem_taxa_all$speciesAccepted,
  stand = stem_taxa_all$plot_id,
  family = stem_taxa_all$familyAccepted,
  addWoodDensityData = if (param$wd_method == "field") { wd_summ },
  verbose = TRUE)

# Check rows of output match stems
stopifnot(
  nrow(wd_all) == nrow(stem_taxa_all),
  identical(as.character(wd_all$genus), as.character(stem_taxa_all$genusAccepted)),
  identical(as.character(wd_all$species), as.character(stem_taxa_all$speciesAccepted)))

# Fill missing wood density SD, e.g. local estimates from a single sample of 
# a taxon not in the global wood density database, with the mean SD of other 
# taxa estimated at the same level
sd_na <- is.na(wd_all$sdWD)
if (any(sd_na)) {
  sd_level <- wd_all %>% 
    filter(!is.na(sdWD)) %>% 
    distinct(genus, species, levelWD, sdWD) %>% 
    group_by(levelWD) %>% 
    summarise(sdWD_fill = mean(sdWD), .groups = "drop")
  sd_fill <- sd_level$sdWD_fill[match(wd_all$levelWD[sd_na], sd_level$levelWD)]
  sd_fill[is.na(sd_fill)] <- mean(sd_level$sdWD_fill)
  wd_all$sdWD[sd_na] <- sd_fill
  message("Filled missing wood density SD for ", sum(sd_na), " stems")
}

# Create output dataframe
wd_out <- cbind(record_id = stem_taxa_all$record_id, wd_all)

# Write to file
write.csv(wd_out, file.path(outdir, "stem_wd.csv"), row.names = FALSE)

# Summarise the taxonomic level of wood density estimates. Estimates from the
# plot mean are used where a stem has no matched genus or family
wd_check <- wd_out %>%
  mutate(level = ifelse(levelWD %in% c("species", "genus", "family", "dataset"),
    levelWD, "plot mean")) %>%
  mutate(level = factor(level, 
    levels = intersect(c("species", "genus", "family", "plot mean", "dataset"), level)))

wd_level <- wd_check %>%
  group_by(level) %>%
  summarise(
    n_stem = n(),
    meanWD_mean = mean(meanWD, na.rm = TRUE),
    meanWD_min = min(meanWD, na.rm = TRUE),
    meanWD_max = max(meanWD, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(pct_stem = round(100 * n_stem / sum(n_stem), 1), .after = n_stem)
msgTable(wd_level, "Wood density (g cm-3) by taxonomic level of the estimate:")

# Figure of wood density at each taxonomic level of the estimate, for stems
# used to estimate AGB density, see `agbdFilter()` in ./func.R
record_agbd <- agbdFilter(stem, plot, recordFilter(stem, plot),
  diam_min_cm = param$agbd_diam_min_cm,
  include_broken = param$include_broken)
wd_fig <- filter(wd_check, as.character(record_id) %in% record_agbd)
p_wd <- wdLevelPlot(wd_fig$meanWD, wd_fig$level) +
  labs(
    title = "Wood density estimates by taxonomic level",
    subtitle = "Stems used to estimate AGBD. Y-axis scales differ")
saveFig(p_wd, file.path(report_dir, "03_wd_level.png"), width = 8,
  height = 2 + 2.2 * nlevels(droplevels(wd_fig$level)))
