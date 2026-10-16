# Format ForestGEO data for GEO-TREES
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Template to convert tree inventory data exported from the ForestGEO data
# portal (https://forestgeo.si.edu) into GEO-TREES L0 data files: `stem.csv`,
# `plot.csv`, and `plot_pt.csv`. ForestGEO plots follow the census protocol
# of Condit (1998), which defines the standard stem codes translated into
# GEO-TREES stem codes. Many sites also use their own codes.
#
# To use this template for a site, copy it to
# ./sites/<SITE>/<ACQUISITION>/01_fmt.R, put the raw data in
# `raw_dir`/`raw_version` defined in param.yaml, and check every line marked
# "TODO". When run by
# ./zz_site.R, `param`, `indir`, `outdir`, and the column templates are
# already defined. To run it on its own, from the repository root, set
# `p_file` below.
#
# Expected raw data files in `indir`:
#   * `PlotDataReport*.txt` - ForestGEO data portal "plot data report"
#       export, tab separated, one row per stem, from a single census. Values
#       recorded in other censuses, e.g. stem coordinates, should already
#       have been copied over
#   * `plot_corners.csv` - optional, plot corner coordinates. One row per
#       point, with columns: `plot_id`, `point_id`, `x_rel_m`, `y_rel_m`,
#       `longitude`, `latitude`. Not provided by the ForestGEO data portal.
#       Without this file, `plot_pt.csv` is not written.

# Setup ------------------------------------------------------------------------

if (!exists("param")) {
  library(dplyr)
  library(yaml)
  source("./func.R")

  p_file <- "./sites/<SITE>/<ACQUISITION>/v1/param.yaml"  # TODO
  param <- read_yaml(p_file)
  indir <- file.path(param$raw_dir, param$raw_version)
  outdir <- file.path(param$out_dir, "01_fmt")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  stem_cols <- read.csv("./templates/stem_cols.csv")
  plot_cols <- read.csv("./templates/plot_cols.csv")
  pt_cols <- read.csv("./templates/pt_cols.csv")
}

# Settings ---------------------------------------------------------------------

# Plot name. Plot data reports contain a single plot, and don't include the
# plot name
plot_name <- "Amacayacu"  # TODO

# Plot dimensions in metres, along the X and Y axes of the stem map
plot_dim <- c(500, 500)  # TODO

# Import data ------------------------------------------------------------------

s_files <- list.files(indir, "^PlotDataReport.*\\.txt$", full.names = TRUE)
if (length(s_files) == 0) {
  stop("No ForestGEO plot data reports found in: ", indir)
}
s <- bind_rows(lapply(s_files, read.delim, colClasses = c(Codes = "character")))

# The data must contain a single census per plot. Values recorded in other
# censuses, e.g. stem coordinates, should already have been copied over
if (length(unique(s$Census)) > 1) {
  stop("Plots with more than one census: ", plot_name)
}

# Stem codes -------------------------------------------------------------------

# Translate network codes into GEO-TREES stem codes, see
# ./templates/stem_codes.csv. Codes mapped to "" are ignored. Codes not in a
# map give a warning: add them with a GEO-TREES code, or "" to ignore them.
# TODO: check codes used in this dataset

# Stem status
status_map <- c(
  "alive" = "A",           # Alive
  "broken below" = "AT",   # Alive, broken below breast height
  "stem dead" = "D",       # Stem dead, other stems of the tree alive
  "dead" = "D",            # Dead
  "missing" = "M")         # Missing

# Standard ForestGEO codes, separated by ";" in `Codes`
code_map <- c(
  A = "",      # POM at alternative height, not 1.3 m
  B = "U",     # Large buttress, requiring ladder to measure
  C = "",      # POM changed since prior census
  I = "I",     # Stem irregular where measured
  L = "",      # Stem leaning
  M = "",      # Multiple stems
  MAIN = "",   # Main stem
  P = "",      # Problem requiring further attention
  Q = "B",     # Stem broken above breast height
  R = "",      # Resprout
  X = "T",     # Stem broken below breast height
  Y = "F",     # Stem prostrate
  DC = "DF",   # Dead, stem fallen
  DN = "DM",   # Presumed dead, no tag or stem
  DS = "DS",   # Dead, stem standing
  DT = "DM")   # Dead, only tag found

# Prepare stem data ------------------------------------------------------------

s_clean <- s %>%
  mutate(
    # DBH is in mm, HOM in m
    diam_cm = as.numeric(DBH) / 10,
    pom_m = as.numeric(HOM),
    height_m = NA_real_,

    alive = Status %in% c("alive", "broken below"),
    code = codeCombine(
      codeMap(Status, status_map, split = NA),
      codeMap(Codes, code_map, split = "[;, ]+"),
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

    # Dates are YYYY-MM-DD
    measurement_date_res = dateRes(Date),
    measurement_date = decDate(Date),

    record_id = row_number(),
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = plot_name,
    subplot_id = as.character(Quadrat),
    tree_id = as.character(Tag),
    stem_id = ifelse(is.na(StemTag) | StemTag == "", as.character(Tag), as.character(StemTag)),
    x_rel_m = as.numeric(PX),
    y_rel_m = as.numeric(PY),
    taxon_name = trimws(paste(Latin, ifelse(is.na(SubSpecies), "", SubSpecies))),
    # Plot data reports don't include family, so palms can't be identified
    growth_form = NA_character_,  # TODO
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    notes = NA_character_) %>%
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

p_clean <- data.frame(plot_id = plot_name) %>%
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = plot_id,
    # ForestGEO plots are surveyed using horizontal distances, i.e. a planar
    # projection
    plot_planar = TRUE,
    plot_length_m = plot_dim[1],
    plot_width_m = plot_dim[2],
    notes_plot = NA_character_,
    # ForestGEO censuses include all stems >= 1 cm diameter
    meas_diam_min_cm = 1,  # TODO
    meas_pom_default_m = 1.3,
    meas_tree_stem = TRUE,
    meas_tree_group = TRUE,
    meas_dead = TRUE,
    meas_fallen = TRUE,
    meas_liana = NA,  # TODO
    meas_palm = NA,  # TODO
    meas_bamboo = NA,  # TODO
    meas_plot_loc = NA_character_,  # TODO
    meas_stem_loc = "Stems mapped relative to 20x20 m quadrat corners",  # TODO
    meas_protocol = "Condit (1998) Tropical Forest Census Plots, ISBN 978-3-662-03664-8") %>%  # TODO
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
