# Format ForestPlots data for GEO-TREES
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Template to convert tree inventory data exported from ForestPlots
# (https://forestplots.net) into GEO-TREES L0 data files: `stem.csv`,
# `plot.csv`, and `plot_pt.csv`. Most ForestPlots data follow the RAINFOR
# field protocol and data codes (https://rainfor.org/en/manuals/), which
# define the stem codes translated into GEO-TREES stem codes.
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
#   * `fp_individuals.csv` - ForestPlots "individuals" export, one row per
#       stem, from a single census per plot. Values recorded in other
#       censuses, e.g. stem coordinates, should already have been copied over
#   * `fp_plots.csv` - ForestPlots plot meta-data export, one row per plot
#   * `plot_corners.csv` - optional, plot corner coordinates. One row per
#       point, with columns: `plot_id`, `point_id`, `x_rel_m`, `y_rel_m`,
#       `longitude`, `latitude`. Not provided by ForestPlots. Without this
#       file, `plot_pt.csv` is not written.

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

# Plots to include, matching `Plot Code`. NULL includes all plots
plot_sel <- NULL  # TODO

# Plot shapes to include, matching `Plot Shape`. The GEO-TREES workflow
# requires contiguous rectangular plots
shape_sel <- c("Square / Rectangular")

# Import data ------------------------------------------------------------------

s <- read.csv(file.path(indir, "fp_individuals.csv"))
p <- read.csv(file.path(indir, "fp_plots.csv"))

# Select plots
if (!is.null(plot_sel)) {
  s <- filter(s, Plot.Code %in% plot_sel)
}
p <- filter(p, Plot.Code %in% s$Plot.Code)

plot_shape_bad <- p$Plot.Code[!p$Plot.Shape %in% shape_sel]
if (length(plot_shape_bad) > 0) {
  message("Plots excluded because of their shape: ",
    paste(plot_shape_bad, collapse = ", "))
  s <- filter(s, !Plot.Code %in% plot_shape_bad)
  p <- filter(p, !Plot.Code %in% plot_shape_bad)
}

plot_dim_bad <- p$Plot.Code[is.na(p$Maximum.Dimension..m.) | is.na(p$Minimum.Dimension..m.)]
if (length(plot_dim_bad) > 0) {
  message("Plots excluded because their dimensions are missing: ",
    paste(plot_dim_bad, collapse = ", "))
  s <- filter(s, !Plot.Code %in% plot_dim_bad)
  p <- filter(p, !Plot.Code %in% plot_dim_bad)
}

# The data must contain a single census per plot. Values recorded in other
# censuses, e.g. stem coordinates, should already have been copied over
plot_census_n <- tapply(s$Census.No, s$Plot.Code, function(x) length(unique(x)))
if (any(plot_census_n > 1)) {
  stop("Plots with more than one census: ",
    paste(names(plot_census_n)[plot_census_n > 1], collapse = ", "))
}

# Stem codes -------------------------------------------------------------------

# Translate network codes into GEO-TREES stem codes, see
# ./templates/stem_codes.csv. Codes mapped to "" are ignored. Codes not in a
# map give a warning: add them with a GEO-TREES code, or "" to ignore them.
# TODO: check codes used in this dataset

# Flag 1 - alive status, single characters, "0" if dead
f1_map <- c(
  "0" = "",   # Dead
  a = "",     # Alive normal
  b = "B",    # Broken stem/top & resprouting
  c = "",     # Leaning
  d = "F",    # Fallen
  e = "I",    # Fluted or fenestrated
  f = "H",    # Hollow
  g = "",     # Rotten or bracket fungus
  h = "",     # Multiple stemmed individual
  i = "",     # No leaves, few leaves
  j = "",     # Burnt
  k = "T",    # Snapped < 1.3 m
  l = "",     # Has liana >= 10 cm diameter
  m = "",     # Covered by lianas
  n = "",     # New recruit
  o = "",     # Lightning damage
  p = "",     # Cut
  q = "",     # Peeling bark
  s = "",     # Has a strangler
  w = "",     # Wound or cambium exposed
  x = "",     # Elephant damage
  y = "",     # Termite damage
  z = "")     # Declining productivity

# Flag 2 - mode of death, first character is the physical mechanism of
# mortality, "1" if alive
f2_map <- c(
  a = "S",    # Standing
  b = "SB",   # Broken (snapped trunk)
  c = "F",    # Uprooted
  d = "S",    # Standing or broken, probably standing
  e = "SB",   # Standing or broken, probably broken
  f = "S",    # Standing or broken
  g = "F",    # Broken or uprooted, probably uprooted
  h = "SB",   # Broken or uprooted, probably broken
  i = "F",    # Broken or uprooted, not standing
  k = "M",    # Vanished
  l = "M",    # Presumed dead, location not found
  m = "")     # Unknown

# Flag 4 - post-field data management. Diameters which were not measured
# directly are described in `notes`. "6" (POM changed) may be combined with
# another code, e.g. "60"
f4_est <- c(
  "1" = "Diameter extrapolated from previous measurements",
  "3" = "Diameter interpolated between measurements",
  "4" = "Diameter estimated from median growth rate",
  "7" = "Diameter assumes zero growth",
  "8" = "Diameter transformed, see ForestPlots notes")

# Prepare stem data ------------------------------------------------------------

s_clean <- s %>%
  # Stems in multi-stemmed trees share a `Stem Group ID`
  mutate(tree_group = ifelse(is.na(Stem.Group.ID),
    paste0("stem", TreeID), paste0("group", Stem.Group.ID))) %>%
  group_by(Plot.Code, tree_group) %>%
  mutate(tree_id = as.character(min(TreeID))) %>%
  ungroup() %>%
  mutate(
    # Diameter (D4, after ForestPlots quality control) and POM are in mm.
    # Zero is used for missing values, 9999 for unknown POM
    diam_cm = ifelse(D4 > 0, D4 / 10, NA_real_),
    pom_m = ifelse(POM > 0 & POM < 9999, POM / 1000, NA_real_),
    height_m = ifelse(Height > 0, Height, NA_real_),

    alive = F1 != "0",
    code = codeCombine(
      ifelse(alive, "A", "D"),
      codeMap(F1, f1_map, split = ""),
      codeMap(ifelse(alive, NA, substr(F2, 1, 1)), f2_map, split = ""),
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

    # Census dates are decimal years. Whole years are assumed to be year only
    measurement_date_res = ifelse(Census.Date == floor(Census.Date),
      "YYYY", "YYYY-MM-DD"),
    measurement_date = ifelse(measurement_date_res == "YYYY",
      Census.Date + 0.5, round(Census.Date, 3)),

    record_id = row_number(),
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = Plot.Code,
    subplot_id = as.character(Standardised.SubPlot.T1),
    stem_id = as.character(TreeID),
    # Standardised coordinates are relative to the plot origin. X and Y are
    # relative to subplots, and are not used
    x_rel_m = as.numeric(Standardised.X),
    y_rel_m = as.numeric(Standardised.Y),
    taxon_name = Recommended.Species,
    growth_form = ifelse(Recommended.Family %in% "Arecaceae", "palm", "tree"),
    height_allometry = NA_character_,
    agb_allometry = NA_character_,
    notes = pasteVals(
      ifelse(Comments == "", NA_character_, Comments),
      unname(f4_est[substr(gsub("6", "", as.character(F4)), 1, 1)]),
      sep = "; ")) %>%
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

# Default POM, the most common POM in each plot
pom_default <- s_clean %>%
  filter(!is.na(pom_m)) %>%
  count(plot_id, pom_m) %>%
  group_by(plot_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

p_clean <- p %>%
  mutate(
    site_id = param$site_id,
    acquisition_id = param$acquisition_id,
    plot_id = Plot.Code,
    # RAINFOR plots sample a fixed area of land surface, not a planar
    # projection
    plot_planar = FALSE,  # TODO
    plot_length_m = as.numeric(Maximum.Dimension..m.),
    plot_width_m = as.numeric(Minimum.Dimension..m.),
    notes_plot = ifelse(Plot.Name == "", NA_character_, Plot.Name),
    meas_diam_min_cm = as.numeric(Minimum.Tree.Diameter.Measured..mm.) / 10,
    meas_pom_default_m = pom_default$pom_m[match(plot_id, pom_default$plot_id)],
    meas_tree_stem = TRUE,
    meas_tree_group = any(!is.na(s$Stem.Group.ID)),
    meas_dead = TRUE,
    meas_fallen = TRUE,
    # Lianas are measured in a separate RAINFOR protocol, and are not
    # included in the individuals export
    meas_liana = NA,  # TODO
    meas_palm = TRUE,
    meas_bamboo = NA,  # TODO
    meas_plot_loc = NA_character_,  # TODO
    meas_stem_loc = NA_character_,  # TODO
    meas_protocol = "https://rainfor.org/en/manuals/",  # TODO
    forest_status = ifelse(Forest.Status == "", NA_character_, Forest.Status)) %>%
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
