# Summarise outputs and processing choices for site reports
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-21

# Writes tables for GEO-TREES site reports to `outdir`, the report directory,
# alongside the figures and files written there by other steps:
#   site_summary.csv - one row per reported value, grouped by section: plot
#     inventory, measurement methods, stem measurement completeness,
#     auxiliary datasets, AGB density, large trees, data quality, provenance
#   plot_summary.csv - one row per plot
#   census_dates.csv - census dates of each plot, with their resolution
#   processing_choices.csv, processing_choices.md - choices made during
#     processing, e.g. the allometry, the sources of height and wood density,
#     and which sources of uncertainty are propagated
#   figures.csv - figures for the report, with suggested captions, and the
#     figures of the GEO-TREES site report which they reproduce
#
# Values describe the stems used to estimate AGB density, see `agbdFilter()`
# in ./func.R, unless stated otherwise. Stem and basal area densities use
# the nominal plot area, as in ./05_agb_stem.R. AGB density uses the area of
# the quadrat grid, as in ./08_agb_mc.R

# Remove previous outputs of this step, so that files which are no longer
# written are not left behind, including figures written by earlier versions
# of this step. Figures and files written to `outdir` by other steps are kept
unlink(list.files(outdir, full.names = TRUE,
  pattern = "^12_.*\\.png$|^(site_summary|plot_summary|census_dates|processing_choices|figures)\\.(csv|md)$"))

# Report settings
# `large_diam_cm`, the diameter thresholds for large trees, is set in 
# ./zz_site.R. The smallest is also used to report large stems measured 
# above the default POM
# Stems measured more than this distance above the default POM of the plot,
# in m, are counted as measured above the default POM, e.g. above buttresses
pom_raised_tol_m <- 0.05
# Figures and panels of the GEO-TREES site report reproduced by figures of
# the processing steps. Update if the figures of the report are renumbered
report_fig <- c(
  "07_plot_location_site.png" = "Figure 5",
  "07_plot_poly.png" = "Figure 5, by plot",
  "06_diam_hist.png" = "Figure 6A",
  "04_height_diam.png" = "Figure 6B",
  "03_wd_level.png" = "Figure 6C",
  "10_quad_agbd_hist.png" = "Figure 7A",
  "10_stem_agb_map_site.png" = "Figure 7B",
  "10_stem_agb_map.png" = "Figure 7B, by plot",
  "10_quad_agbd_map_site.png" = "Figure 7C",
  "10_quad_agbd_map.png" = "Figure 7C, by plot",
  "10_quad_agbd_interval_site.png" = "Figure 7D")

# Levels of taxonomic resolution and wood density estimates
wd_lev <- c("species", "genus", "family", "plot mean", "dataset")
res_lev <- c("YYYY", "YYYY-MM", "YYYY-MM-DD")

# Coarsest resolution of a set of dates
resCoarse <- function(x) {
  i <- match(x, res_lev)
  if (all(is.na(i))) "YYYY-MM-DD" else res_lev[min(i, na.rm = TRUE)]
}

# Summarise a plot meta-data flag across plots
measFlag <- function(x) {
  x <- as.logical(x)
  if (all(is.na(x))) {
    "unknown"
  } else if (all(x %in% TRUE)) {
    "yes"
  } else if (all(x %in% FALSE)) {
    "no"
  } else {
    "some plots"
  }
}

# One row of the site summary. Repeated values, e.g. the same plot
# dimensions in every plot, are reported once
sRow <- function(section, field, value, unit = NA_character_) {
  value <- value[!is.na(value) & value != ""]
  data.frame(
    section = section,
    field = field,
    value = if (length(value) == 0) NA_character_ else
      paste(unique(as.character(value)), collapse = "; "),
    unit = unit)
}

# One row of the processing choices table
cRow <- function(step, choice, parameter, value, description) {
  data.frame(
    step = step,
    choice = choice,
    parameter = parameter,
    value = if (all(is.na(value))) NA_character_ else
      paste(as.character(value), collapse = ", "),
    description = description)
}

# Maximum, or NA if there are no values
maxNA <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

# Number of simulations used in error propagation, see ./08_agb_mc.R
n_sim <- length(unique(quad_agb_simu$sim))

# Optional inputs which may be absent
if (is.null(stem_outside)) {
  stem_outside <- data.frame(plot_id = character(0), action = character(0))
}
if (is.null(tree_unmapped)) {
  tree_unmapped <- data.frame(plot_id = character(0), agb_Mg_mean = numeric(0))
}

# Plot meta-data
plot_meta <- plot %>%
  mutate(
    plot_id = as.character(plot_id),
    area_ha_nominal = plot_width_m * plot_length_m / 10000,
    diam_min_agbd_cm = pmax(meas_diam_min_cm, param$agbd_diam_min_cm))

# Stems used to estimate AGB density
stem_df <- st_drop_geometry(stem_summ) %>%
  mutate(plot_id = as.character(plot_id))
stem_agbd <- stem_df %>%
  filter(in_quadrat_calc %in% TRUE) %>%
  left_join(., dplyr::select(plot_meta, plot_id, meas_pom_default_m),
    by = "plot_id") %>%
  mutate(
    # Stems measured above the default POM, e.g. above buttresses
    pom_raised = !is.na(pom_m) & !is.na(meas_pom_default_m) &
      pom_m > meas_pom_default_m + pom_raised_tol_m,
    level_wd = ifelse(levelWD %in% wd_lev, levelWD, "plot mean"),
    level_taxon = case_when(
      !is.na(speciesAccepted) & speciesAccepted != "" ~ "species",
      !is.na(genusAccepted) & genusAccepted != "" ~ "genus",
      !is.na(familyAccepted) & familyAccepted != "" ~ "family",
      TRUE ~ "undetermined"),
    # Taxa are counted by accepted name, so that original names which
    # resolve to the same taxon are counted once
    taxon_key = case_when(
      level_taxon == "species" ~ paste(genusAccepted, speciesAccepted),
      level_taxon == "genus" ~ genusAccepted,
      level_taxon == "family" ~ familyAccepted,
      TRUE ~ taxon_name))

# Large stems, to report large stems measured above the default POM
large_pom_cm <- min(large_diam_cm)
large_pom <- stem_agbd$diam_cm >= large_pom_cm

# Trees used to estimate AGB density. Multi-stemmed trees use their
# equivalent diameter, as in ./05_agb_stem.R
tree_include <- if (param$multi_stem_method == "equivalent") stem_agbd$record_id else character(0)
tree_agbd <- stem_agbd %>%
  left_join(., treeEquiv(stem_agbd, tree_include)[, c("record_id", "tree_key", "diam_eq_cm")],
    by = "record_id") %>%
  group_by(plot_id, tree_key) %>%
  summarise(
    diam_eq_cm = first(diam_eq_cm),
    agb_Mg = sum(agb_Mg, na.rm = TRUE),
    .groups = "drop")

# Census dates of each plot, and time between census and ALS acquisition.
# Positive offsets are censuses after the ALS acquisition
als_dec <- if (!is.null(param$als_date)) decDate(param$als_date) else NA_real_
census_plot <- stem_agbd %>%
  filter(!is.na(measurement_date)) %>%
  group_by(plot_id) %>%
  summarise(
    census_date_min = min(measurement_date),
    census_date_median = median(measurement_date),
    census_date_max = max(measurement_date),
    census_date_res = resCoarse(measurement_date_res),
    .groups = "drop") %>%
  mutate(als_offset_yr = census_date_median - als_dec)

# Table of census dates of each plot: the earliest, median and latest
# measurement dates of stems used to estimate AGB density, at the coarsest
# resolution of the dates in the plot
census_tab <- census_plot %>%
  left_join(., count(stem_agbd, plot_id, name = "n_stem"), by = "plot_id") %>%
  transmute(
    plot_id,
    n_stem,
    census_date_min = decFormat(census_date_min, census_date_res),
    census_date_median = decFormat(census_date_median, census_date_res),
    census_date_max = decFormat(census_date_max, census_date_res),
    census_date_res) %>%
  arrange(plotFactor(plot_id))

# Plot summary
stem_plot <- stem_agbd %>%
  group_by(plot_id) %>%
  summarise(
    n_stem = n(),
    ba_m2 = sum(ba_m2, na.rm = TRUE),
    pct_stem_pom_raised = 100 * mean(pom_raised),
    "pct_stem_ge{large_pom_cm}_pom_raised" := if (any(diam_cm >= large_pom_cm)) {
      100 * mean(pom_raised[diam_cm >= large_pom_cm])
    } else {
      NA_real_
    },
    pct_stem_wd_plot_mean = 100 * mean(level_wd %in% c("plot mean", "dataset")),
    .groups = "drop")

# Number of large trees, and their share of AGB, for each diameter threshold
tree_plot <- tree_agbd %>%
  group_by(plot_id) %>%
  summarise(
    n_tree = n(),
    agb_Mg_tree_max = max(agb_Mg),
    .groups = "drop") %>%
  left_join(., bind_rows(lapply(large_diam_cm, function(d) {
    tree_agbd %>%
      group_by(plot_id) %>%
      summarise(
        diam = d,
        n_tree_ge = sum(diam_eq_cm >= d, na.rm = TRUE),
        pct_agb_ge = 100 * sum(agb_Mg[diam_eq_cm >= d]) / sum(agb_Mg),
        .groups = "drop")
  })) %>%
    pivot_wider(names_from = diam, values_from = c(n_tree_ge, pct_agb_ge),
      names_sep = ""), by = "plot_id")

# AGB of stems used to estimate AGB, in each plot, including stems below
# the minimum diameter threshold, to compare with trees without coordinates
agb_plot <- stem_df %>%
  filter(in_agb_calc %in% TRUE) %>%
  group_by(plot_id) %>%
  summarise(agb_Mg_all = sum(agb_Mg, na.rm = TRUE), .groups = "drop")

outside_plot <- stem_outside %>%
  group_by(plot_id) %>%
  summarise(
    n_stem_outside_assigned = sum(action != "excluded"),
    n_stem_outside_excluded = sum(action == "excluded"),
    .groups = "drop")

unmapped_plot <- tree_unmapped %>%
  group_by(plot_id) %>%
  summarise(
    n_tree_unmapped = n(),
    agb_Mg_unmapped = sum(agb_Mg_mean, na.rm = TRUE),
    .groups = "drop")

plot_summ <- plot_meta %>%
  dplyr::select(plot_id, plot_width_m, plot_length_m, area_ha_nominal,
    diam_min_agbd_cm) %>%
  left_join(., census_plot, by = "plot_id") %>%
  left_join(., dplyr::select(poly_check, plot_id, area_ha_poly, area_diff_pct,
    n_gnss, n_gnss_outlier, any_of(c("grid_excl_pct", "n_stem_grid_excl"))),
    by = "plot_id") %>%
  left_join(., stem_plot, by = "plot_id") %>%
  left_join(., tree_plot, by = "plot_id") %>%
  left_join(., dplyr::select(plot_agb, plot_id, n_quadrat, area_ha_grid = area_ha,
    agbd_Mg_ha_stem_sum, agbd_Mg_ha_mc_mean, agbd_Mg_ha_mc_sd,
    agbd_Mg_ha_mc_ci2.5, agbd_Mg_ha_mc_ci97.5, agbd_cv_pct = cv_pct,
    agbd_diff_pct = diff_pct), by = "plot_id") %>%
  left_join(., outside_plot, by = "plot_id") %>%
  left_join(., unmapped_plot, by = "plot_id") %>%
  left_join(., agb_plot, by = "plot_id") %>%
  mutate(
    # Plots with no stems used to estimate AGB density
    across(c(n_stem_outside_assigned, n_stem_outside_excluded, n_tree_unmapped,
      n_stem, n_tree, starts_with("n_tree_ge")), ~ifelse(is.na(.x), 0L, .x)),
    ba_m2 = ifelse(is.na(ba_m2), 0, ba_m2),
    pct_agb_unmapped = 100 * ifelse(is.na(agb_Mg_unmapped), 0, agb_Mg_unmapped) /
      agb_Mg_all,
    n_stem_ha = n_stem / area_ha_nominal,
    ba_m2_ha = ba_m2 / area_ha_nominal,
    across(starts_with("n_tree_ge"), ~.x / area_ha_nominal, .names = "{.col}_ha"),
    census_date_min = decFormat(census_date_min, census_date_res),
    census_date_median = decFormat(census_date_median, census_date_res),
    census_date_max = decFormat(census_date_max, census_date_res)) %>%
  dplyr::select(-ba_m2, -agb_Mg_unmapped, -agb_Mg_all) %>%
  mutate(across(where(is.double), ~round(.x, 2))) %>%
  arrange(plotFactor(plot_id))

# AGB density over all plots, from the area-weighted mean of quadrat AGB
# density in each simulation
quad_df <- st_drop_geometry(quad_summ) %>%
  mutate(
    plot_id = as.character(plot_id),
    quadrat_id = as.character(quadrat_id),
    cv_pct = 100 * agbd_Mg_ha_mc_sd / agbd_Mg_ha_mc_mean)
site_simu <- quad_agb_simu %>%
  inner_join(., dplyr::select(quad_df, plot_id, quadrat_id, quadrat_area_ha),
    by = c("plot_id", "quadrat_id")) %>%
  group_by(sim) %>%
  summarise(
    agbd_Mg_ha = sum(agbd_Mg_ha * quadrat_area_ha) / sum(quadrat_area_ha),
    .groups = "drop")
site_agbd <- c(
  mean = mean(site_simu$agbd_Mg_ha),
  sd = sd(site_simu$agbd_Mg_ha),
  ci2.5 = unname(quantile(site_simu$agbd_Mg_ha, 0.025)),
  ci97.5 = unname(quantile(site_simu$agbd_Mg_ha, 0.975)))

# Taxonomic resolution and wood density level
taxa_res <- stem_agbd %>%
  group_by(level_taxon) %>%
  summarise(
    n_name = n_distinct(taxon_key, na.rm = TRUE),
    n_stem = n(),
    .groups = "drop") %>%
  mutate(pct_stem = 100 * n_stem / sum(n_stem))
taxaVal <- function(level, col) {
  x <- taxa_res[[col]][taxa_res$level_taxon == level]
  if (length(x) == 0) 0 else round(x, 1)
}

wd_res <- stem_agbd %>%
  group_by(level_wd) %>%
  summarise(
    n_stem = n(),
    agb_Mg = sum(agb_Mg, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(pct_agb = 100 * agb_Mg / sum(agb_Mg))
wdVal <- function(level, col) {
  x <- wd_res[[col]][wd_res$level_wd == level]
  if (length(x) == 0) 0 else round(x, 1)
}

# Field height measurements, in a height table or in the stem table
n_height_obs <- if (!is.null(height)) {
  sum(!is.na(height$height_m))
} else {
  sum(!is.na(stem_df$height_m))
}

# Repeat diameter measurements used to estimate measurement error
n_dup_used <- if (!is.null(diam_dup)) sum(diam_dup$status == "used") else 0

# Height model, if fitted to field measurements
height_mod_used <- if (!is.null(height_mod_comp)) {
  height_mod_comp[height_mod_comp$used %in% TRUE, ][1, ]
} else {
  NULL
}

# Software versions
bm_desc <- utils::packageDescription("BIOMASS")
bm_version <- paste0(bm_desc$Version,
  if (!is.null(bm_desc$RemoteSha)) {
    paste0(" (", bm_desc$RemoteRef, ", ", substr(bm_desc$RemoteSha, 1, 7), ")")
  })

# Descriptions of processing choices, also used in the site summary
allom_desc <- if (param$height_method == "field") {
  paste0("Chave et al. (2014) pantropical Eq. 4, AGB (kg) = 0.0673 x ",
    "(WD x D^2 x H)^0.976, using heights from the local height-diameter model")
} else {
  paste0("Chave et al. (2014) pantropical Eq. 7, AGB (kg) = exp(-2.024 - ",
    "0.896 E + 0.920 ln(WD) + 2.795 ln(D) - 0.0461 ln(D)^2), where E is the ",
    "bioclimatic stress variable at the plot centre. Height is not used")
}

height_desc <- if (param$height_method == "field") {
  paste0("Local height-diameter model fitted to ", height_mod_used$n_height,
    " field height measurements with BIOMASS::modelHD(): ",
    height_mod_used$method, " model, RSE ", round(height_mod_used$RSE, 2), " m. ",
    if (param$height_model == "best") {
      "Chosen as the model with the lowest RSE"
    } else {
      "Chosen by the user"
    }, ". Heights are predicted for all stems from this model")
} else {
  paste0("No local height data used. Stem heights reported in the outputs, ",
    "e.g. quadrat maximum height, are predicted from diameter with Chave et ",
    "al. (2014) Eq. 6a at the plot centre, and are not used to estimate AGB")
}

wd_desc <- paste0(
  if (param$wd_method == "field") {
    paste0("Local wood density measurements (", nrow(wd), " records), ",
      "combined with the Global Wood Density Database (Chave et al. 2009; ",
      "Zanne et al. 2009), with BIOMASS::getWoodDensity(). ")
  } else {
    paste0("Global Wood Density Database (Chave et al. 2009; Zanne et al. ",
      "2009), with BIOMASS::getWoodDensity(). ")
  },
  "Each stem uses the estimate at the lowest taxonomic level available: ",
  "species (", wdVal("species", "n_stem"), " stems), genus (",
  wdVal("genus", "n_stem"), "), family (", wdVal("family", "n_stem"),
  "), otherwise the plot mean (", wdVal("plot mean", "n_stem") +
    wdVal("dataset", "n_stem"), ")")

diam_error_desc <- if (identical(param$diam_error, "none")) {
  "Not propagated"
} else if (is.numeric(param$diam_error)) {
  paste0("Fixed standard deviation of ", param$diam_error, " cm for all stems")
} else if (identical(param$diam_error, "field") && !is.null(diam_error_fit)) {
  paste0("Estimated from ", diam_error_fit$n_pairs, " repeat diameter ",
    "measurements with BIOMASS::fitDiamError(): SD = ", signif(diam_error_fit$a, 3),
    " + ", signif(diam_error_fit$b, 3), " D cm, with large errors (SD ",
    signif(diam_error_fit$sd_large, 3), " cm) in ",
    signif(100 * diam_error_fit$p_large, 2), "% of measurements",
    if (!diam_error_fit$large_estimated) {
      ". Too few repeat measurements to estimate large errors, those of Chave et al. (2004) are used"
    })
} else {
  paste0(
    if (identical(param$diam_error, "field")) {
      "Too few repeat diameter measurements to estimate measurement error. "
    },
    "Chave et al. (2004): SD = 0.0904 + 0.0062 D cm for 95% of ",
    "measurements, and SD = 4.64 cm for 5%")
}

polygon_desc <- c(
  exact = "Plot corners are the mean of the GNSS measurements of each corner",
  affine = paste0("An affine transformation from plot coordinates to GNSS ",
    "coordinates is fitted by least squares, allowing for independent scaling ",
    "and shearing"),
  procrustes = paste0("A Procrustes fit (rotation and translation only) of ",
    "the plot coordinates to the GNSS coordinates, which keeps the shape and ",
    "dimensions of the plot"))[param$polygon_method]

# Site summary
taxa_unmatched <- taxa_outcome[taxa_outcome$outcome == "not matched", ]
site_summ <- bind_rows(
  # Plot inventory
  sRow("Plot inventory", "Number of plots", nrow(plot_meta)),
  sRow("Plot inventory", "Number of plots used for AGBD estimation", nrow(plot_agb)),
  sRow("Plot inventory", "Plot dimensions",
    paste(plot_meta$plot_width_m, "x", plot_meta$plot_length_m), "m"),
  sRow("Plot inventory", "Total nominal plot area",
    round(sum(plot_meta$area_ha_nominal), 2), "ha"),
  sRow("Plot inventory", "Total plot area used for AGBD estimation",
    round(sum(plot_agb$area_ha), 2), "ha"),
  sRow("Plot inventory", "Census dates", if (nrow(census_plot) > 0) paste(
    decFormat(min(census_plot$census_date_min), resCoarse(census_plot$census_date_res)),
    "to",
    decFormat(max(census_plot$census_date_max), resCoarse(census_plot$census_date_res)))),
  sRow("Plot inventory", "Median census date", if (nrow(census_plot) > 0) decFormat(
    median(stem_agbd$measurement_date, na.rm = TRUE),
    resCoarse(census_plot$census_date_res))),
  sRow("Plot inventory", "ALS acquisition date", param$als_date),
  sRow("Plot inventory", "Median time from ALS acquisition to census",
    round(median(census_plot$als_offset_yr), 2), "years"),
  sRow("Plot inventory", "Maximum time between ALS acquisition and census",
    round(maxNA(abs(census_plot$als_offset_yr)), 2), "years"),
  sRow("Plot inventory", "Number of stem records", nrow(stem_df)),
  sRow("Plot inventory", "Number of stems used for AGBD estimation", nrow(stem_agbd)),
  sRow("Plot inventory", "Number of trees used for AGBD estimation", nrow(tree_agbd)),
  sRow("Plot inventory", "Number of families", 
    n_distinct(stem_agbd$familyAccepted, na.rm = TRUE)),
  sRow("Plot inventory", "Number of genera", 
    n_distinct(stem_agbd$genusAccepted, na.rm = TRUE)),
  sRow("Plot inventory", "Number of species", n_distinct(
    paste(stem_agbd$genusAccepted, stem_agbd$speciesAccepted)[!is.na(stem_agbd$speciesAccepted)])),

  # Measurement methods
  sRow("Measurement methods", "Minimum stem diameter measurement threshold",
    sort(unique(plot_meta$meas_diam_min_cm)), "cm"),
  sRow("Measurement methods", "Minimum stem diameter threshold used in AGBD calculations",
    sort(unique(plot_meta$diam_min_agbd_cm)), "cm"),
  sRow("Measurement methods", "Default point of measurement (POM)",
    sort(unique(plot_meta$meas_pom_default_m)), "m"),
  sRow("Measurement methods", "Stems measured above the default POM",
    round(100 * mean(stem_agbd$pom_raised), 1), "% of stems"),
  sRow("Measurement methods",
    paste0("Stems >= ", large_pom_cm, " cm measured above the default POM"),
    if (any(large_pom)) round(100 * mean(stem_agbd$pom_raised[large_pom]), 1),
    "% of stems"),
  sRow("Measurement methods", "Measurement protocol", plot_meta$meas_protocol),
  sRow("Measurement methods", "Stem mapping method", plot_meta$meas_stem_loc),
  sRow("Measurement methods", "Plot geo-location measurement method", plot_meta$meas_plot_loc),
  sRow("Measurement methods", "Stem height", height_desc),
  sRow("Measurement methods", "Above-ground woody biomass allometry", allom_desc),
  sRow("Measurement methods", "Wood density", wd_desc),
  sRow("Measurement methods", "Species-level taxonomic IDs", taxaVal("species", "n_name"), "names"),
  sRow("Measurement methods", "Genus-level taxonomic IDs", taxaVal("genus", "n_name"), "names"),
  sRow("Measurement methods", "Family-level taxonomic IDs", taxaVal("family", "n_name"), "names"),
  sRow("Measurement methods", "Stems identified to species", taxaVal("species", "pct_stem"), "% of stems"),
  sRow("Measurement methods", "Stems identified to genus only", taxaVal("genus", "pct_stem"), "% of stems"),
  sRow("Measurement methods", "Stems identified to family only", taxaVal("family", "pct_stem"), "% of stems"),
  sRow("Measurement methods", "Stems undetermined", taxaVal("undetermined", "pct_stem"), "% of stems"),
  sRow("Measurement methods", "Species-level wood density estimates", wdVal("species", "n_stem"), "stems"),
  sRow("Measurement methods", "Genus-level wood density estimates", wdVal("genus", "n_stem"), "stems"),
  sRow("Measurement methods", "Family-level wood density estimates", wdVal("family", "n_stem"), "stems"),
  sRow("Measurement methods", "Plot mean wood density estimates",
    wdVal("plot mean", "n_stem") + wdVal("dataset", "n_stem"), "stems"),
  sRow("Measurement methods", "AGB from stems with species-level wood density",
    wdVal("species", "pct_agb"), "% of AGB"),
  sRow("Measurement methods", "AGB from stems with genus-level wood density",
    wdVal("genus", "pct_agb"), "% of AGB"),
  sRow("Measurement methods", "AGB from stems with family-level wood density",
    wdVal("family", "pct_agb"), "% of AGB"),
  sRow("Measurement methods", "AGB from stems with plot mean wood density",
    wdVal("plot mean", "pct_agb") + wdVal("dataset", "pct_agb"), "% of AGB"),

  # Stem measurement completeness. Dead and fallen stems are never used to
  # estimate AGB
  sRow("Stem measurement completeness", "Live stems", "yes"),
  sRow("Stem measurement completeness", "Every stem of multi-stemmed trees",
    measFlag(plot_meta$meas_tree_stem)),
  sRow("Stem measurement completeness", "Dead stems", measFlag(plot_meta$meas_dead)),
  sRow("Stem measurement completeness", "Fallen stems", measFlag(plot_meta$meas_fallen)),
  sRow("Stem measurement completeness", "Lianas", measFlag(plot_meta$meas_liana)),
  sRow("Stem measurement completeness", "Palms", measFlag(plot_meta$meas_palm)),
  sRow("Stem measurement completeness", "Bamboo", measFlag(plot_meta$meas_bamboo)),

  # Auxiliary datasets
  sRow("Auxiliary datasets", "Field-measured height-diameter data",
    if (n_height_obs == 0) "no" else paste0("yes (", n_height_obs, " measurements)")),
  sRow("Auxiliary datasets", "Local height-diameter allometry used",
    if (param$height_method == "field") "yes" else "no"),
  sRow("Auxiliary datasets", "Field-measured wood density data",
    if (is.null(wd)) "no" else paste0("yes (", nrow(wd), " records)")),
  sRow("Auxiliary datasets", "Local wood density data used",
    if (param$wd_method == "field") "yes" else "no"),
  sRow("Auxiliary datasets", "Repeat diameter measurements",
    if (is.null(diam_dup)) "no" else paste0("yes (", n_dup_used, " pairs used)")),
  sRow("Auxiliary datasets", "Taxon table, e.g. families of unmatched names",
    if (is.null(taxon)) "no" else "yes"),

  # AGB density
  sRow("AGB density", "AGBD, all plots", round(site_agbd["mean"], 1), "Mg/ha"),
  sRow("AGB density", "AGBD, all plots, 95% interval",
    paste(round(site_agbd["ci2.5"], 1), "to", round(site_agbd["ci97.5"], 1)), "Mg/ha"),
  sRow("AGB density", "AGBD, all plots, coefficient of variation",
    round(100 * site_agbd["sd"] / site_agbd["mean"], 1), "%"),
  sRow("AGB density", "AGBD, range of plot means",
    paste(round(min(plot_agb$agbd_Mg_ha_mc_mean), 1), "to",
      round(max(plot_agb$agbd_Mg_ha_mc_mean), 1)), "Mg/ha"),
  sRow("AGB density", "Basal area, all plots",
    round(sum(stem_agbd$ba_m2, na.rm = TRUE) / sum(plot_meta$area_ha_nominal), 1), "m2/ha"),
  sRow("AGB density", "Stem density, all plots",
    round(nrow(stem_agbd) / sum(plot_meta$area_ha_nominal)), "stems/ha"),
  sRow("AGB density", "Quadrat dimensions",
    paste(param$quad_dim[1], "x", param$quad_dim[2]), "m"),
  sRow("AGB density", "Number of quadrats", nrow(quad_df)),
  sRow("AGB density", "Quadrat AGBD, median",
    round(median(quad_df$agbd_Mg_ha_mc_mean), 1), "Mg/ha"),
  sRow("AGB density", "Quadrat AGBD, range",
    paste(round(min(quad_df$agbd_Mg_ha_mc_mean), 1), "to",
      round(max(quad_df$agbd_Mg_ha_mc_mean), 1)), "Mg/ha"),
  sRow("AGB density", "Quadrat AGBD, median coefficient of variation",
    round(median(quad_df$cv_pct, na.rm = TRUE), 1), "%"),

  # Large trees
  bind_rows(lapply(large_diam_cm, function(d) {
    bind_rows(
      sRow("Large trees", paste0("Trees >= ", d, " cm diameter"),
        round(sum(tree_agbd$diam_eq_cm >= d) / sum(plot_meta$area_ha_nominal), 2),
        "trees/ha"),
      sRow("Large trees", paste0("AGB in trees >= ", d, " cm diameter"),
        round(100 * sum(tree_agbd$agb_Mg[tree_agbd$diam_eq_cm >= d]) /
          sum(tree_agbd$agb_Mg), 1), "% of AGB"))
  })),
  sRow("Large trees", "Largest tree diameter", round(max(tree_agbd$diam_eq_cm), 1), "cm"),
  sRow("Large trees", "Largest tree AGB", round(max(tree_agbd$agb_Mg), 1), "Mg"),

  # Data quality
  bind_rows(lapply(seq_len(nrow(record_status)), function(i) {
    sRow("Data quality", paste0("Stem records ", record_status$status[i]),
      record_status$n_record[i], "records")
  })),
  sRow("Data quality", "Taxonomic names not matched",
    if (nrow(taxa_unmatched) > 0) taxa_unmatched$n_name else 0, "names"),
  sRow("Data quality", "Stems with taxonomic names not matched",
    if (nrow(taxa_unmatched) > 0) taxa_unmatched$n_stem else 0, "stems"),
  sRow("Data quality", "GNSS points", sum(poly_check$n_gnss), "points"),
  sRow("Data quality", "GNSS points removed as outliers", sum(poly_check$n_gnss_outlier), "points"),
  sRow("Data quality", "Largest difference between fitted and nominal plot area",
    round(maxNA(abs(poly_check$area_diff_pct)), 1), "%"),
  sRow("Data quality", "Plot area left out of the quadrat grid",
    if (!is.null(poly_check$area_ha_quad)) {
      round(100 * (1 - sum(poly_check$area_ha_quad) / sum(poly_check$area_ha_poly)), 1)
    }, "%"),
  sRow("Data quality", "Stems in plot area left out of the quadrat grid, excluded",
    sum(poly_check$n_stem_grid_excl), "stems"),
  sRow("Data quality", "Stems outside the quadrat grid, assigned to the nearest quadrat",
    sum(stem_outside$action != "excluded"), "stems"),
  sRow("Data quality", "Stems outside the quadrat grid, excluded",
    sum(stem_outside$action == "excluded"), "stems"),
  sRow("Data quality", "Trees without coordinates", nrow(tree_unmapped), "trees"),
  sRow("Data quality", "AGB in trees without coordinates",
    round(100 * sum(tree_unmapped$agb_Mg_mean, na.rm = TRUE) /
      sum(stem_df$agb_Mg[stem_df$in_agb_calc %in% TRUE], na.rm = TRUE), 1), "% of AGB"),
  sRow("Data quality", "Largest difference between simulated plot AGBD and sum of stem AGB",
    round(maxNA(abs(plot_agb$diff_pct)), 1), "%"),
  sRow("Data quality", "Diameter measurement error", diam_error_desc),

  # Provenance
  sRow("Provenance", "Site ID", param$site_id),
  sRow("Provenance", "Acquisition ID", param$acquisition_id),
  sRow("Provenance", "Product version", param$product_version),
  sRow("Provenance", "Raw data version", param$raw_version),
  sRow("Provenance", "INV_processing version", param$software_version),
  sRow("Provenance", "BIOMASS version", bm_version),
  sRow("Provenance", "R version", paste(R.version$major, R.version$minor, sep = ".")),
  sRow("Provenance", "Processing date", format(Sys.Date())),
  sRow("Provenance", "Data products", list.files(file.path(ver_dir, c("L1", "L2", "L3")),
    pattern = "\\.(csv|gpkg)$")))

# Processing choices
choice <- bind_rows(
  cRow("Stem selection", "Stems used to estimate AGB", NA, NA,
    paste0("Stems which are alive, standing, have a diameter measurement, and ",
      "are not broken below the POM or missing. Where a stem was measured more ",
      "than once, the measurement at the highest POM, then the latest date, ",
      "then the largest diameter, is used")),
  cRow("Stem selection", "Minimum diameter", "agbd_diam_min_cm", param$agbd_diam_min_cm,
    paste0("Stems below the larger of this and the measurement threshold of ",
      "the plot (", paste(sort(unique(plot_meta$diam_min_agbd_cm)), collapse = ", "),
      " cm) have AGB estimates, but are not included in AGB density")),
  cRow("Stem selection", "Broken stems", "include_broken", param$include_broken,
    if (param$include_broken) {
      "Stems broken above the POM are included, using the allometry for intact stems"
    } else {
      "Stems broken above the POM are excluded from AGB density"
    }),
  cRow("Stem selection", "Multi-stemmed trees", "multi_stem_method", param$multi_stem_method,
    if (param$multi_stem_method == "equivalent") {
      paste0("AGB is estimated for each tree from its equivalent diameter, ",
        "sqrt(sum(D^2)), then shared among its stems in proportion to their ",
        "basal area")
    } else {
      "AGB is estimated for each stem separately"
    }),
  cRow("Taxonomy", "Taxonomic name standardisation", NA, NA,
    paste0("Names are matched to World Flora Online with BIOMASS::correctTaxo(), ",
      "preferring accepted names. ", sum(taxa_outcome$n_name[taxa_outcome$outcome == "matched, changed"]),
      " of ", sum(taxa_outcome$n_name), " names were changed, and ",
      if (nrow(taxa_unmatched) > 0) taxa_unmatched$n_name else 0, " were not matched",
      if (!is.null(taxon)) ". Families of unmatched names are taken from the taxon table supplied by the site")),
  cRow("Wood density", "Wood density source", "wd_method", param$wd_method, wd_desc),
  cRow("Height", "Stem height", "height_method", param$height_method, height_desc),
  cRow("Allometry", "Above-ground woody biomass allometry", "height_method",
    param$height_method, paste0(allom_desc, ". Allometries supplied for ",
      "individual stems (`agb_allometry` in the stem table) are not used")),
  cRow("Plot geometry", "Plot polygon", "polygon_method", param$polygon_method,
    paste0(polygon_desc, ", using BIOMASS::plotPolygonFit()")),
  cRow("Plot geometry", "GNSS outliers", "corner_outlier_dist_m", param$corner_outlier_dist_m,
    paste0("GNSS points more than ", param$corner_outlier_dist_m, " m from their ",
      "fitted position are removed as outliers")),
  cRow("Plot geometry", "Quadrats", "quad_dim", param$quad_dim,
    paste0("Plots are divided into ", param$quad_dim[1], " x ", param$quad_dim[2],
      " m quadrats with BIOMASS::divide_plot(). ",
      if (identical(param$quad_origin, "centre")) {
        paste0("The quadrat grid is centred in each plot, so where the plot ",
          "dimensions are not a multiple of the quadrat dimensions, equal ",
          "strips along opposite edges of the plot are left out")
      } else if (identical(param$quad_origin, "corner")) {
        paste0("The quadrat grid starts at the origin corner of each plot, so ",
          "where the plot dimensions are not a multiple of the quadrat ",
          "dimensions, strips along the far edges of the plot are left out")
      } else {
        paste0("The quadrat grid starts ", param$quad_origin[1], " m and ",
          param$quad_origin[2], " m from the origin corner of each plot, ",
          "in x and y, and strips of the plot outside the grid are left out")
      },
      ". Stems in strips left out are excluded from AGB density. Up to ", 
      100 * param$quad_grid_tol, "% of the area of each plot (`quad_grid_tol`) ",
      "can be left out",
      if (any(poly_check$grid_excl_pct > 0.5, na.rm = TRUE)) {
        paste0(". ", round(100 * (1 - sum(poly_check$area_ha_quad) / 
          sum(poly_check$area_ha_poly)), 1), "% of the plot area was left out")
      })),
  cRow("Plot geometry", "Stems outside the quadrat grid", "outside_max_dist_m",
    param$outside_max_dist_m,
    if (param$outside_max_dist_m > 0) {
      paste0("Trees whose stem centroid is outside the quadrat grid by up to ",
        param$outside_max_dist_m, " m are assigned to the nearest quadrat. Others ",
        "are excluded from AGB density")
    } else {
      paste0("Trees whose stem centroid is outside the quadrat grid are ",
        "excluded from AGB density")
    }),
  cRow("Plot geometry", "Trees without coordinates", "unmapped_method", param$unmapped_method,
    if (param$unmapped_method == "random") {
      paste0("Trees without coordinates are assigned to a random quadrat of ",
        "their plot in each simulation, with probability proportional to ",
        "quadrat area, so their AGB is kept in the plot and their location ",
        "adds to the quadrat AGBD uncertainty")
    } else {
      "Trees without coordinates are excluded from AGB density"
    }),
  cRow("AGB density", "Plot area", NA, NA,
    paste0("AGB density is estimated over the area of the fitted quadrat grid. ",
      "Plot AGB density is the area-weighted mean of quadrat AGB density")),
  cRow("Figures", "Stems shown in figures", NA, NA,
    paste0("Figures of stem diameter, height, wood density and AGB show the ",
      "stems used to estimate AGB density. The figure of stem height against ",
      "diameter shows measured heights, if any, and heights predicted for ",
      "these stems, ", if (param$height_method == "field") {
        paste0("from the local height-diameter model (", height_mod_used$method, 
          "), with the fitted model")
      } else {
        "with Chave et al. (2014) Eq. 6a, which are not used to estimate AGB"
      })),
  cRow("Uncertainty", "Error propagation", NA, NA,
    paste0("Errors are propagated to each tree in ", n_sim, " simulations with ",
      "BIOMASS::AGBmonteCarlo(), then summed in quadrats with ",
      "BIOMASS::subplot_summary(). Uncertainty is the standard deviation and ",
      "95% interval of the simulations")),
  cRow("Uncertainty", "Diameter measurement error", "diam_error", param$diam_error,
    diam_error_desc),
  cRow("Uncertainty", "Wood density error", NA, NA,
    "Standard deviation of the wood density estimate at its taxonomic level"),
  cRow("Uncertainty", "Height error", "height_method", param$height_method,
    if (param$height_method == "field") {
      "Residual standard error of the local height-diameter model"
    } else {
      "Not propagated separately, height is not used in Chave et al. (2014) Eq. 7"
    }),
  cRow("Uncertainty", "Allometric model error", NA, NA,
    paste0("Residual error and coefficient uncertainty of the Chave et al. ",
      "(2014) model, from posterior samples of its parameters")),
  cRow("Uncertainty", "Not propagated", NA, NA,
    paste0("Errors in the GNSS coordinates of plot corners, which need ",
      "repeated measurements of each corner; errors in stem coordinates; ",
      "taxonomic misidentification; and systematic diameter errors, e.g. ",
      "stems measured around buttresses")))

# Write processing choices as Markdown, for reading alongside the data
choice_md <- c(
  paste("# Processing choices:", param$site_id, param$acquisition_id,
    param$product_version),
  "",
  paste0("INV_processing ", param$software_version, ", BIOMASS ", bm_version,
    ", processed ", format(Sys.Date()), "."),
  unlist(lapply(unique(choice$step), function(x) {
    ch <- choice[choice$step == x, ]
    c("", paste("##", x), "", paste0("- **", ch$choice, "**",
      ifelse(is.na(ch$parameter), "", paste0(" (`", ch$parameter, ": ", ch$value, "`)")),
      ": ", ch$description, "."))
  })))

# Figures for the report, written to `outdir` by other steps, with suggested
# captions. `report_figure` is the figure of the GEO-TREES site report which
# each one reproduces, if any, see `report_fig`
fRow <- function(file, section, caption) {
  data.frame(file = file, section = section, caption = caption)
}
allom_eq <- if (param$height_method == "field") {
  "Chave et al. (2014) Eq. 4" 
} else {
  "Chave et al. (2014) Eq. 7"
}
quad_txt <- paste0(param$quad_dim[1], " x ", param$quad_dim[2], " m quadrats")
height_caption <- if (param$height_method == "field") {
  paste0("measured heights, and heights predicted for all stems by the local ",
    "height-diameter model (", height_mod_used$method, ")")
} else {
  paste0("measured heights, if any, and heights predicted from diameter with ",
    "Chave et al. (2014) Eq. 6a, which are not used to estimate AGB")
}
fig_index <- bind_rows(
  fRow("07_plot_location_site.png", "Plot inventory", paste0("Plot locations. ",
    "GNSS measurements of plot corners (blue), and plot polygons fitted to ",
    "them (black, ", param$polygon_method, " method)")),
  fRow("07_plot_poly.png", "Plot inventory", paste0("Plot polygons fitted to ",
    "GNSS points (", param$polygon_method, " method), outlines of the GNSS ",
    "corners, and ", quad_txt, ", in each plot")),
  fRow("07_stem_map.png", "Plot inventory", paste0("Stem locations in each ",
    "plot, with ", quad_txt, ", in plot coordinates. Stems outside the plot, ",
    "and stems in strips left out of the quadrat grid, are highlighted")),
  fRow("06_diam_hist.png", "Stem size distribution", 
    "Diameter of stems used to estimate AGB density"),
  fRow("04_height_diam.png", "Stem size distribution", 
    paste0("Stem height against diameter: ", height_caption)),
  fRow("03_wd_level.png", "Stem size distribution", 
    "Wood density estimates at each taxonomic level of the estimate"),
  fRow("05_agb_diam.png", "AGB density", 
    paste0("Tree AGB against diameter, from ", allom_eq)),
  fRow("05_agb_diam_class.png", "AGB density", 
    "Share of AGB in each diameter class"),
  fRow("08_plot_agbd.png", "AGB density", paste0("Plot AGBD, with the 95% ",
    "interval of ", n_sim, " simulations, and the sum of stem AGB")),
  fRow("10_quad_agbd_hist.png", "AGB density", paste0("Distribution of ",
    "quadrat AGB density (AGBD) in ", quad_txt, ", coloured as in the maps ",
    "of quadrat AGBD")),
  fRow("10_stem_agb_map_site.png", "AGB density", 
    paste0("Stem AGB across the site, from ", allom_eq, ", with quadrats")),
  fRow("10_stem_agb_map.png", "AGB density", 
    paste0("Stem AGB in each plot, from ", allom_eq, ", with quadrats. ",
      "Axis ranges differ between plots")),
  fRow("10_quad_agbd_map_site.png", "AGB density", 
    paste0("Quadrat AGBD across the site, mean of ", n_sim, " simulations")),
  fRow("10_quad_agbd_map.png", "AGB density", 
    paste0("Quadrat AGBD in each plot, mean of ", n_sim, " simulations. ",
      "Axis ranges differ between plots")),
  fRow("10_quad_agbd_compare.png", "AGB density", paste0("Quadrat AGBD from ",
    n_sim, " simulations against the sum of stem AGB")),
  fRow("10_quad_agbd_interval_site.png", "AGB density uncertainty", 
    paste0("Quadrat AGBD, mean and 95% interval of ", n_sim, " simulations, ",
      "all quadrats ranked by AGBD")),
  fRow("10_quad_agbd_interval.png", "AGB density uncertainty", 
    paste0("Quadrat AGBD, mean and 95% interval of ", n_sim, " simulations, ",
      "quadrats ranked within each plot")),
  fRow("10_quad_agbd_cv_map.png", "AGB density uncertainty", 
    paste0("Coefficient of variation of quadrat AGBD in each plot, from ", 
      n_sim, " simulations. Axis ranges differ between plots")),
  fRow("10_quad_agbd_cv.png", "AGB density uncertainty", 
    paste0("Coefficient of variation of quadrat AGBD against quadrat AGBD, ",
      "from ", n_sim, " simulations")),
  fRow("08_diam_error.png", "AGB density uncertainty", paste0("Differences ",
    "between repeat diameter measurements, with the fitted measurement error ",
    "model"))) %>%
  mutate(report_figure = unname(report_fig[file]), .after = file) %>%
  filter(file.exists(file.path(outdir, file)))

# Write to file
write.csv(site_summ, file.path(outdir, "site_summary.csv"), row.names = FALSE)
write.csv(plot_summ, file.path(outdir, "plot_summary.csv"), row.names = FALSE)
write.csv(census_tab, file.path(outdir, "census_dates.csv"), row.names = FALSE)
write.csv(choice, file.path(outdir, "processing_choices.csv"), row.names = FALSE)
writeLines(choice_md, file.path(outdir, "processing_choices.md"))
write.csv(fig_index, file.path(outdir, "figures.csv"), row.names = FALSE)

msgTable(site_summ[site_summ$section %in% c("AGB density", "Large trees"),
  c("field", "value", "unit")], "Site summary for reports:")
