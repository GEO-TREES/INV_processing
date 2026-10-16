# Clean Paracou stem data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-16

# Raw data are Guyafor exports, one file per plot containing all censuses.
# See GuyaforDataDictionary.pdf in `indir` for column definitions and codes

# Census year used for each plot. Plot 16 is censused every five years
census_sel <- data.frame(
  plot_id = as.character(1:16),
  census_year = c(rep(2019, 15), 2020))

# Import stem data
s_files <- list.files(indir, pattern = "AllYears\\.csv$", recursive = TRUE,
  full.names = TRUE)
s_list <- lapply(s_files, read.csv)

# Check all columns are identical
stopifnot(all(vapply(s_list, function(x) {
  identical(names(x), names(s_list[[1]]))
}, logical(1))))

# Select one census per plot
s <- bind_rows(s_list) %>%
  mutate(
    Plot = as.character(Plot),
    taxon_name = paste(trimws(GenusFilled), trimws(SpeciesFilled))) %>% 
  inner_join(., census_sel, by = c("Plot" = "plot_id", "CensusYear" = "census_year"))

plot_miss <- setdiff(census_sel$plot_id, s$Plot)
if (length(plot_miss) > 0) {
  stop("Selected census not found for plots: ", paste(plot_miss, collapse = ", "))
}

# Import plot metadata, one row per subplot
plot_meta <- read.csv(file.path(indir, "ParacouDescription.csv")) %>%
  filter(Plot %in% census_sel$plot_id)

# Translate Guyafor tree state into GEO-TREES stem codes, see
# ./templates/stem_codes.csv. Tree state is defined by `CodeAlive` and
# `MeasCode`, joined as "CodeAlive_MeasCode"
state_map <- c(
  "1_0" = "",    # Alive, measured at 1.3 m or above buttresses
  "1_1" = "",    # Alive, POM raised by 0.5 m since last measurement
  "1_2" = "",    # Alive, POM raised by 1 m since last measurement
  "1_3" = "",    # Alive, POM raised by 1.5 m since last measurement
  "1_4" = "I",   # Alive, irregular trunk, circumference estimated
  "1_7" = "",    # Alive, damaged by logging
  "1_9" = "",    # Alive, poisoned as part of silvicultural treatment
  "1_11" = "",   # Alive, natural damage
  "1_12" = "F",  # Alive, lying
  "0_0" = "S",   # Standing dead, natural death
  "0_1" = "S",   # Standing dead, following forest treatment
  "0_4" = "F",   # Logged and commercially exploited
  "0_5" = "",    # Destroyed by logging operations
  "0_6" = "F",   # Natural death, primary treefall
  "0_7" = "F",   # Natural death, secondary treefall
  "0_9" = "",    # Death following poisoning
  "0_11" = "")   # Death following natural damage

# Tree states described in `notes`
state_note <- c(
  "1_7" = "Damaged by logging",
  "1_9" = "Poisoned as part of silvicultural treatment",
  "1_11" = "Natural damage",
  "0_1" = "Death following forest treatment",
  "0_4" = "Logged",
  "0_5" = "Destroyed by logging operations",
  "0_6" = "Primary treefall",
  "0_7" = "Secondary treefall",
  "0_9" = "Death following poisoning",
  "0_11" = "Death following natural damage")

# Ratio of circumferences in the Nogueira et al. (2006) correction applied by
# Guyafor to the fluted trunks of Swartzia polyphylla, `CorrCode` "+5"
nogueira_ratio <- with(s[s$CorrCode %in% "0 +5", ], median(CircCorr / Circ, na.rm = TRUE))

# Prepare stem data
s_clean <- s %>%
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = Plot,
    subplot_id = as.character(SubPlot),
    tree_id = as.character(idTree),
    stem_id = as.character(idTree),
    # Uncertain census dates are treated as year only
    measurement_date_res = ifelse(CensusDateCertainty == 1, "YYYY-MM-DD", "YYYY"),
    measurement_date = ifelse(CensusDateCertainty == 1,
      decDate(CensusDate), decDate(CensusYear)),
    alive = CodeAlive == 1,
    state = paste(CodeAlive, MeasCode, sep = "_"),
    code = codeCombine(
      ifelse(alive, "A", "D"),
      codeMap(state, state_map, split = NA),
      standing = alive),
    # Corrected circumference is used where available, see `CorrCode`,
    # except where the series was realigned (`CorrCode` 2 or 4) after the POM
    # was raised. There the correction estimates the circumference at the
    # original POM, i.e. around the buttress, while allometries assume
    # diameter measured above buttresses, so the measured circumference is
    # used. The Nogueira correction for fluted trunks is kept
    pom_realign = grepl("^[24]", CorrCode) & !is.na(POM) & POM > 130 &
      !is.na(Circ),
    circ = case_when(
      is.na(CircCorr) ~ Circ,
      pom_realign & grepl("+5", CorrCode, fixed = TRUE) ~ Circ * nogueira_ratio,
      pom_realign ~ Circ,
      TRUE ~ CircCorr),
    # Circumference of dead trees is carried over from the last census, so
    # is not a measurement
    diam_cm = ifelse(alive, circ / pi, NA_real_),
    pom_m = ifelse(alive, POM / 100, NA_real_),
    height_m = NA_real_,
    growth_form = ifelse(FamilyFilled %in% "Arecaceae", "palm", "tree"),
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    notes = pasteVals(
      unname(state_note[state]),
      case_when(
        is.na(CorrCode) | CorrCode == "0" ~ NA_character_,
        pom_realign ~ paste("Measured circumference used, series realigned",
          "after POM raised, Guyafor CorrCode:", CorrCode),
        TRUE ~ paste("Circumference corrected, Guyafor CorrCode:", CorrCode)),
      sep = "; "),
    # Stem coordinates are relative to the plot origin, except in plot 16,
    # where they are relative to the origin of each 100x100 m subplot.
    # Subplots are numbered west to east, then north to south, checked
    # against subplot corner coordinates in ParacouDescription.csv
    col = (SubPlot - 1) %% 5,
    row = 4 - ((SubPlot - 1) %/% 5),
    x_rel_m = ifelse(plot_id == "16", Xfield + 100 * col, Xfield),
    y_rel_m = ifelse(plot_id == "16", Yfield + 100 * row, Yfield)) %>%
  mutate(record_id = row_number()) %>%
  colGen(., stem_cols$column_name, stem_cols$class) %>%
  dplyr::select(all_of(stem_cols$column_name))

# Prepare taxon table. Stems not identified to genus have placeholder names,
# e.g. "Indet.Moraceae Indet.", which cannot be matched, so the family is 
# used to estimate wood density
taxon_clean <- s %>% 
  distinct(taxon_name, FamilyFilled) %>% 
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    taxon_family = na_if(trimws(FamilyFilled), "Indet.")) %>% 
  colGen(., taxon_cols$column_name, taxon_cols$class) %>% 
  dplyr::select(all_of(taxon_cols$column_name))

# Prepare plot corners, plots are square
pt_clean <- plot_meta %>%
  distinct(Plot, PlotArea, across(matches("^Plot[XY]utm"))) %>%
  pivot_longer(
    cols = matches("^Plot[XY]utm"),
    names_to = c(".value", "point_id"),
    names_pattern = "Plot([XY]utm)(SW|SE|NE|NW)") %>%
  mutate(
    site_id = param$site_id,
    plot_id = Plot,
    plot_dim_m = sqrt(PlotArea * 10000),
    x_rel_m = ifelse(point_id %in% c("SE", "NE"), plot_dim_m, 0),
    y_rel_m = ifelse(point_id %in% c("NW", "NE"), plot_dim_m, 0),
    rover_easting_utm_m = Xutm,
    rover_northing_utm_m = Yutm,
    crs_epsg = 32622,
    crs_name = "UTM 22N") %>%
  ptFormat(., pt_cols)

# Prepare plot meta-data
p_clean <- plot_meta %>%
  distinct(Plot, PlotArea, Treatment, LoggingDate) %>%
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = Plot,
    plot_planar = TRUE,
    plot_length_m = sqrt(PlotArea * 10000),
    plot_width_m = sqrt(PlotArea * 10000),
    meas_diam_min_cm = 10,
    meas_pom_default_m = 1.3,
    meas_tree_stem = TRUE,
    meas_tree_group = TRUE,
    meas_dead = TRUE,
    meas_fallen = TRUE,
    meas_liana = TRUE,
    meas_palm = TRUE,
    meas_bamboo = TRUE,
    forest_status = ifelse(Treatment == "Natural_Forest", "old-growth", "logged"),
    treatment = ifelse(Treatment == "Natural_Forest", NA_character_,
      paste0("Paracou disturbance experiment: ", gsub("_", " ", Treatment),
        ", logged in ", LoggingDate))) %>%
  colGen(., plot_cols$column_name, plot_cols$class) %>%
  dplyr::select(all_of(plot_cols$column_name))

# Check all columns in output objects
colCheck(p_clean, plot_cols)
colCheck(pt_clean, pt_cols)
colCheck(s_clean, stem_cols)
colCheck(taxon_clean, taxon_cols)

# Check values
valCheck(
  plot = p_clean,
  stem = s_clean,
  pt = pt_clean,
  taxon = taxon_clean)

# Write corner points to file
write.csv(pt_clean, file.path(outdir, "plot_pt.csv"), row.names = FALSE)

# Write plot meta-data to file
write.csv(p_clean, file.path(outdir, "plot.csv"), row.names = FALSE)

# Write stem data to file
write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)

# Write taxon table to file
write.csv(taxon_clean, file.path(outdir, "taxon.csv"), row.names = FALSE)
