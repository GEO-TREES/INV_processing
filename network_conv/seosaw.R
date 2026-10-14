# Format SEOSAW data for GEO-TREES
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Template to convert tree inventory data from the SEOSAW database
# (https://seosaw.github.io) into GEO-TREES L0 data files: `stem.csv`,
# `plot.csv`, and `plot_pt.csv`. SEOSAW data follow the SEOSAW field manual
# and dataset manual, which define the stem codes translated into GEO-TREES
# stem codes.
#
# To use this template for a site, copy it to
# ./sites/<SITE>/<ACQUISITION>/01_fmt.R, put the raw data in the `raw_dir`
# defined in param.yaml, and check every line marked "TODO". When run by
# ./zz_site.R, `param`, `indir`, `outdir`, and the column templates are
# already defined. To run it on its own, from the repository root, set
# `p_file` below.
#
# Expected raw data files in `indir`:
#   * `stems.csv` - SEOSAW stem table, one row per stem, from a single
#       census per plot. Values recorded in other censuses, e.g. stem
#       coordinates, should already have been copied over
#   * `plots.csv` - SEOSAW plot table, one row per plot, from the same census
#       as `stems.csv`
#   * `plot_corners.csv` - optional, plot corner coordinates. One row per
#       point, with columns: `plot_id`, `point_id`, `x_rel_m`, `y_rel_m`,
#       `longitude`, `latitude`. SEOSAW plot polygons (`polys.shp`) may be
#       used to create this file. Without this file, `plot_pt.csv` is not
#       written.

# Setup ------------------------------------------------------------------------

if (!exists("param")) {
  library(dplyr)
  library(yaml)
  source("./func.R")

  p_file <- "./sites/<SITE>/<ACQUISITION>/v1/param.yaml"  # TODO
  param <- read_yaml(p_file)
  indir <- param$raw_dir
  outdir <- file.path(param$out_dir, "01_fmt")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  stem_cols <- read.csv("./templates/stem_cols.csv")
  plot_cols <- read.csv("./templates/plot_cols.csv")
  pt_cols <- read.csv("./templates/pt_cols.csv")
}

# Settings ---------------------------------------------------------------------

# Plots to include, matching `plot_id`. NULL includes all plots
plot_sel <- NULL  # TODO

# Plot shapes to include, matching `plot_shape`. The GEO-TREES workflow
# requires contiguous rectangular plots
shape_sel <- c("rectangle")

# Import data ------------------------------------------------------------------

s <- read.csv(file.path(indir, "stems.csv"))
p <- read.csv(file.path(indir, "plots.csv"))

# Select plots
if (!is.null(plot_sel)) {
  s <- filter(s, plot_id %in% plot_sel)
}
p <- filter(p, plot_id %in% s$plot_id)

plot_shape_bad <- p$plot_id[!p$plot_shape %in% shape_sel]
if (length(plot_shape_bad) > 0) {
  message("Plots excluded because of their shape: ",
    paste(plot_shape_bad, collapse = ", "))
  s <- filter(s, !plot_id %in% plot_shape_bad)
  p <- filter(p, !plot_id %in% plot_shape_bad)
}

plot_dim_bad <- p$plot_id[is.na(p$plot_length) | is.na(p$plot_width)]
if (length(plot_dim_bad) > 0) {
  message("Plots excluded because their dimensions are missing: ",
    paste(plot_dim_bad, collapse = ", "))
  s <- filter(s, !plot_id %in% plot_dim_bad)
  p <- filter(p, !plot_id %in% plot_dim_bad)
}

# The data must contain a single census per plot. Values recorded in other
# censuses, e.g. stem coordinates, should already have been copied over
plot_census_n <- tapply(s$census_date, s$plot_id, function(x) length(unique(x)))
if (any(plot_census_n > 1)) {
  stop("Plots with more than one census: ",
    paste(names(plot_census_n)[plot_census_n > 1], collapse = ", "))
}

if (any(duplicated(p$plot_id))) {
  stop("Plots with more than one row in `plots.csv`: ",
    paste(unique(p$plot_id[duplicated(p$plot_id)]), collapse = ", "))
}

# Stem codes -------------------------------------------------------------------

# Translate network codes into GEO-TREES stem codes, see
# ./templates/stem_codes.csv. Codes mapped to "" are ignored. Codes not in a
# map give a warning: add them with a GEO-TREES code, or "" to ignore them.
# TODO: check codes used in this dataset

# Stem status
status_map <- c(
  a = "A",    # Alive
  r = "AT",   # Resprouting, dead above POM but alive elsewhere on the tree
  d = "D")    # Dead

# Stem mode, single characters
mode_map <- c(
  b = "B",    # Broken, material above break detached
  c = "",     # Climbing or strangling plants in canopy
  f = "F",    # Fallen
  g = "H",    # Hollow
  j = "",     # Bark peeling or lost
  k = "",     # Bracket fungi
  l = "",     # Leaning > 15 degrees
  m = "",     # On a termite mound
  n = "",     # New recruit
  p = "B",    # Snapped, material above break attached
  q = "M",    # Stem location unknown, not found
  s = "S",    # Standing
  t = "T",    # Stump, broken below POM
  u = "F",    # Uprooted
  v = "M",    # Vanished, location found but stem missing
  w = "",     # Liana, see `growth_form`
  y = "",     # Wounded
  z = "")     # Sick or rotten wood

# Prepare stem data ------------------------------------------------------------

s_clean <- s %>%
  mutate(
    # Diameter is in cm, POM and height in m
    diam_cm = as.numeric(diam),
    pom_m = as.numeric(pom),
    height_m = as.numeric(height),

    alive = stem_status %in% c("a", "r"),
    code = codeCombine(
      codeMap(stem_status, status_map, split = ""),
      codeMap(stem_mode, mode_map, split = ""),
      standing = alive),

    # Stems broken below the POM with a diameter were measured below the
    # break, so are broken above the POM
    code = ifelse(grepl("T", code) & !is.na(diam_cm),
      codeCombine(gsub("T", "B", code)), code),

    # Missing stems have no measurements
    missing = grepl("M", code),
    diam_cm = ifelse(missing, NA_real_, diam_cm),
    pom_m = ifelse(missing, NA_real_, pom_m),
    height_m = ifelse(missing, NA_real_, height_m),

    # Census dates are YYYY-MM-DD, YYYY-MM, or YYYY
    measurement_date_res = dateRes(census_date),
    measurement_date = decDate(census_date),

    record_id = row_number(),
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = plot_id,
    subplot_id = as.character(subplot_id),
    tree_id = as.character(tree_id),
    stem_id = as.character(stem_id),
    # Stem locations measured with GPS (`longitude`, `latitude`) are not
    # converted to coordinates relative to the plot origin
    x_rel_m = as.numeric(x_grid),  # TODO
    y_rel_m = as.numeric(y_grid),  # TODO
    taxon_name = species_name_clean,
    growth_form = case_when(
      grepl("w", stem_mode) ~ "liana",
      family_gnr %in% "Arecaceae" ~ "palm",
      TRUE ~ "tree"),
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    notes = ifelse(notes_stem == "", NA_character_, notes_stem)) %>%
  dplyr::select(all_of(stem_cols$column_name))

# Alive stems without coordinates cannot be assigned to quadrats
plot_no_xy <- s_clean %>%
  filter(grepl("A", code)) %>%
  group_by(plot_id) %>%
  summarise(prop_xy = mean(!is.na(x_rel_m) & !is.na(y_rel_m))) %>%
  filter(prop_xy < 0.9)
if (nrow(plot_no_xy) > 0) {
  warning("Plots with coordinates for < 90% of alive stems: ",
    paste0(plot_no_xy$plot_id, " (", round(plot_no_xy$prop_xy * 100), "%)", collapse = ", "),
    call. = FALSE)
}

# Prepare plot data ------------------------------------------------------------

p_clean <- p %>%
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = plot_id,
    # SEOSAW plots sample a fixed area of land surface, not a planar
    # projection
    plot_planar = FALSE,  # TODO
    plot_length_m = as.numeric(plot_length),
    plot_width_m = as.numeric(plot_width),
    notes_plot = ifelse(plot_notes == "", NA_character_, plot_notes),
    meas_diam_min_cm = as.numeric(min_diam_thresh),
    meas_pom_default_m = as.numeric(pom_default),
    meas_tree_stem = meas_tree_stem == "stem",
    meas_tree_group = as.logical(tree_diff),
    meas_dead = as.logical(dead_stems_sampled),
    meas_fallen = TRUE,
    meas_liana = as.logical(lianas_sampled),
    meas_palm = NA,  # TODO
    meas_bamboo = NA,  # TODO
    meas_plot_loc = NA_character_,  # TODO
    meas_stem_loc = xy_method,
    meas_protocol = "https://seosaw.github.io",  # TODO
    land_use = ifelse(land_use_notes == "", NA_character_, land_use_notes)) %>%
  colGen(., plot_cols$column_name, plot_cols$class) %>%
  dplyr::select(all_of(plot_cols$column_name))

# Prepare plot corner points ---------------------------------------------------

corner_file <- file.path(indir, "plot_corners.csv")
if (file.exists(corner_file)) {
  pt_clean <- read.csv(corner_file) %>%
    filter(plot_id %in% p_clean$plot_id) %>%
    mutate(site_id = param$site_id) %>%
    ptFormat(., pt_cols)
} else {
  pt_clean <- NULL
  warning("No plot corner coordinates found, `plot_pt.csv` not written: ",
    corner_file, call. = FALSE)
}

# Check and write data ---------------------------------------------------------

colCheck(s_clean, stem_cols)
colCheck(p_clean, plot_cols)
if (!is.null(pt_clean)) {
  colCheck(pt_clean, pt_cols)
}

valCheck(
  plot = p_clean,
  stem = s_clean,
  pt = pt_clean)

write.csv(s_clean, file.path(outdir, "stem.csv"), row.names = FALSE)
write.csv(p_clean, file.path(outdir, "plot.csv"), row.names = FALSE)
if (!is.null(pt_clean)) {
  write.csv(pt_clean, file.path(outdir, "plot_pt.csv"), row.names = FALSE)
}
