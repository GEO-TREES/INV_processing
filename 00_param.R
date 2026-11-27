# Read and check the parameters of one product version of an acquisition
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-11-27

# Run by `runSite()` in ./run.R, before the processing steps. Reads the
# parameters file `p_file`, checks its values and sets defaults of optional 
# parameters. Creates `param`, the AGB allometries of the site, the column 
# classes of the data tables, and the output directories

# Load YAML file with parameters for one product version of an acquisition
p <- yaml::read_yaml(p_file)

# Acquisition directory, ./sites/<SITE>/<ACQUISITION>/, containing 01_fmt.R
acq_dir <- dirname(dirname(p_file))

# Load YAML file with software version
software_version <- read_yaml("./version.yaml")

# Merge parameters lists
param <- c(p, software_version)

# Define parameter names
param_name_vec <- c(
  "site_id",
  "acquisition_id",
  "quad_dim",
  "opt_s3",
  "s3_dir",
  "out_dir",
  "raw_dir",
  "raw_version",
  "height_method",
  "wd_method",
  "polygon_method",
  "outside_max_dist_m",
  "corner_outlier_dist_m",
  "quad_grid_tol",
  "height_model",
  "agbd_diam_min_cm",
  "include_broken",
  "multi_stem_method",
  "unmapped_method",
  "diam_error",
  "software_version",
  "product_version",
  "rocrate"
)

# Define optional parameter names
# `als_date` is the date of the airborne lidar acquisition matched to the
# plot data, YYYY-MM-DD, YYYY-MM or YYYY, used in ./12_report.R
# `quad_origin` is the alignment of the quadrat grid in each plot, see
# ./07_quad.R: "centre" (default), "corner", or an offset [x, y] in metres
# from the origin corner of each plot
# `missing_diam_method` is how live stems without a diameter measurement are
# treated, see ./08_agb_mc.R: "impute" (default) or "exclude"
# `taper_method` is the correction of diameters measured away from 1.3 m to
# their equivalent at 1.3 m, see `taperB()` in ./func.R: "none" (default),
# "cushman2014" (Barro Colorado Island only) or "cushman2021"
# `taper_species` is the species terms of "cushman2014", by taxon name, e.g.
# {"Ceiba pentandra": 0.0495}. Other species use the base class
# `stem_allometry` is whether stems use the AGB allometry in `agb_allometry`
# in the stem table, see `agb_allometries` in ./func.R: false (default) or
# true
# `corner_error` is the error of the GNSS coordinates of plot corners, which
# is propagated to quadrat area, see ./07_quad.R: "none" (default), 
# "residual", estimated from the fit of the plot polygons, or a standard 
# deviation in m
# `agb_method` is the AGB allometry of trees without their own allometry:
# "regional" (default), Chave et al. (2014), or "custom"
# `agb_custom` is the AGB allometry used if `agb_method` is "custom", an
# equation or a model fitted with brms, see `agbCustom()` in ./func.R
# `stem_allometry_custom` is a named list of allometries, in the format of
# `agb_custom`, which stems can use if `stem_allometry` is true
# `growth_form_exclude` is values of `growth_form` in the stem table excluded
# from AGB density, e.g. ["strangler"]. None by default
# `height_custom` is the height-diameter model used if `height_method` is
# "custom", see `heightCustom()` in ./func.R
# `crown_method` is whether quadrat AGB density is also estimated with tree
# AGB spread over crowns, see ./08_agb_mc.R: "none" (default) or "custom"
# `crown_custom` is the crown area allometry used if `crown_method` is
# "custom", see `crownCustom()` in ./func.R
param_optional_vec <- c(
  "als_date",
  "quad_origin",
  "missing_diam_method",
  "taper_method",
  "taper_species",
  "stem_allometry",
  "corner_error",
  "agb_method",
  "agb_custom",
  "stem_allometry_custom",
  "growth_form_exclude",
  "height_custom",
  "crown_method",
  "crown_custom"
)

# Define RO-Crate parameter names, `author_orcid` is optional
rocrate_name_vec <- c(
  "author_name",
  "author_email",
  "affiliation",
  "license"
)

# Check all parameters present
# `s3_dir` is only needed if `opt_s3` is TRUE
param_missing <- setdiff(param_name_vec, names(param))
if (isFALSE(param$opt_s3)) {
  param_missing <- setdiff(param_missing, "s3_dir")
}
if (length(param_missing) > 0) {
  stop("Missing parameters in ", p_file, ": ", paste(param_missing, collapse = ", "))
}

rocrate_missing <- setdiff(rocrate_name_vec, names(param$rocrate))
if (length(rocrate_missing) > 0) {
  stop("Missing `rocrate` parameters in ", p_file, ": ", paste(rocrate_missing, collapse = ", "))
}

param_extra <- setdiff(names(param), c(param_name_vec, param_optional_vec))
if (length(param_extra) > 0) {
  warning("Unrecognised parameters in ", p_file, ": ", paste(param_extra, collapse = ", "))
}

# Check parameter values
param_choices <- list(
  height_method = c("regional", "field", "custom"),
  wd_method = c("regional", "field", "local"),
  polygon_method = c("exact", "affine", "procrustes"),
  height_model = c("best", "log1", "log2", "weibull", "michaelis"),
  multi_stem_method = c("equivalent", "stem"),
  unmapped_method = c("random", "exclude")
)
for (i in names(param_choices)) {
  if (!param[[i]] %in% param_choices[[i]]) {
    stop("`", i, "` must be one of: ", paste(param_choices[[i]], collapse = ", "))
  }
}

if (length(param$quad_dim) != 2 || !is.numeric(param$quad_dim)) {
  stop("`quad_dim` must be two numbers, e.g. [50, 50]")
}

if (length(param$raw_version) != 1 || !grepl("^[A-Za-z0-9-]+$", param$raw_version)) {
  stop("`raw_version` must be a single version string, e.g. \"v1\", ",
    "without underscores or dots")
}

if (length(param$outside_max_dist_m) != 1 || !is.numeric(param$outside_max_dist_m) ||
  param$outside_max_dist_m < 0) {
  stop("`outside_max_dist_m` must be a number >= 0")
}

if (length(param$corner_outlier_dist_m) != 1 || !is.numeric(param$corner_outlier_dist_m) ||
  param$corner_outlier_dist_m <= 0) {
  stop("`corner_outlier_dist_m` must be a number > 0")
}

if (length(param$quad_grid_tol) != 1 || !is.numeric(param$quad_grid_tol) ||
  param$quad_grid_tol < 0 || param$quad_grid_tol >= 1) {
  stop("`quad_grid_tol` must be a number >= 0 and < 1")
}

if (length(param$agbd_diam_min_cm) != 1 || !is.numeric(param$agbd_diam_min_cm) ||
  param$agbd_diam_min_cm < 0) {
  stop("`agbd_diam_min_cm` must be a number >= 0")
}

if (length(param$include_broken) != 1 || !is.logical(param$include_broken) ||
  is.na(param$include_broken)) {
  stop("`include_broken` must be true or false")
}

if (length(param$diam_error) != 1 ||
  !(param$diam_error %in% c("chave2004", "field", "none") ||
    (is.numeric(param$diam_error) && param$diam_error > 0))) {
  stop("`diam_error` must be \"chave2004\", \"field\", \"none\", or a ",
    "number > 0, the standard deviation of diameter measurement error in cm")
}

if (is.null(param$quad_origin)) {
  param$quad_origin <- "centre"
}
if (is.null(param$missing_diam_method)) {
  param$missing_diam_method <- "impute"
}
if (!identical(param$missing_diam_method, "impute") &&
  !identical(param$missing_diam_method, "exclude")) {
  stop("`missing_diam_method` must be one of: impute, exclude")
}

if (is.null(param$taper_method)) {
  param$taper_method <- "none"
}
if (!param$taper_method %in% c("none", "cushman2014", "cushman2021")) {
  stop("`taper_method` must be one of: none, cushman2014, cushman2021")
}
if (!is.null(param$taper_species) && (!is.list(param$taper_species) ||
  is.null(names(param$taper_species)) ||
  !all(vapply(param$taper_species, is.numeric, logical(1))))) {
  stop("`taper_species` must be taxon names with numeric species terms, ",
    "e.g. {\"Ceiba pentandra\": 0.0495}")
}

param$growth_form_exclude <- unlist(param$growth_form_exclude)
if (length(param$growth_form_exclude) > 0 && !is.character(param$growth_form_exclude)) {
  stop("`growth_form_exclude` must be values of `growth_form`, e.g. [\"strangler\"]")
}

if (param$height_method == "custom") {
  if (is.null(param$height_custom)) {
    stop("`height_method` is \"custom\", so `height_custom` must give the ",
      "height-diameter model's `formula`, `parameters` and `sigma`")
  }
  # Check the model can be built
  invisible(heightCustom(param$height_custom))
}

# AGB allometries, see `agbAllometryParam()` in ./func.R. Trees use Chave et 
# al. (2014) if `agb_method` is "regional" (default), or the allometry in 
# `agb_custom` if it is "custom", as with `height_method` and `height_custom`. 
# If `stem_allometry` is true, stems with a value of `agb_allometry` in the 
# stem table use that allometry instead
if (!is.null(param$agb_custom) && is.null(param$agb_method)) {
  stop("`agb_custom` is given, so `agb_method` must be \"custom\" to use it, ",
    "or \"regional\" to use Chave et al. (2014)")
}
agb_allom <- agbAllometryParam(param, agb_allometries)
agb_allometries <- agb_allom$allometries
agb_allom_default <- agb_allom$default
param$agb_method <- agb_allom$method
param$stem_allometry <- agb_allom$by_stem
if (param$agb_method == "custom") {
  message("AGB allometry of the site: ", agb_allometries$custom$description,
    " (", nrow(agb_allometries$custom$parameters), " set(s) of parameter values)")
}

# A local height-diameter model is fitted as a Bayesian model with the brms
# package, so that the uncertainty in its parameters is propagated to AGB,
# see `heightBayes()` in ./func.R
if (param$height_method == "field" && !requireNamespace("brms", quietly = TRUE)) {
  stop("`height_method` is \"field\", so the local height-diameter model is ",
    "fitted as a Bayesian model with the brms package, which is not installed")
}

if (is.null(param$corner_error)) {
  param$corner_error <- "none"
}
if (length(param$corner_error) != 1 || 
  !(param$corner_error %in% c("none", "residual") ||
    (is.numeric(param$corner_error) && !is.na(param$corner_error) && 
      param$corner_error >= 0))) {
  stop("`corner_error` must be \"none\", \"residual\", or a standard ",
    "deviation in m >= 0")
}

if (is.null(param$crown_method)) {
  param$crown_method <- "none"
}
if (!param$crown_method %in% c("none", "custom")) {
  stop("`crown_method` must be one of: none, custom")
}
if (param$crown_method == "custom") {
  if (is.null(param$crown_custom)) {
    stop("`crown_method` is \"custom\", so `crown_custom` must give the ",
      "crown area allometry's `formula`, `parameters` and `sigma`")
  }
  # Check the allometry can be built
  invisible(crownCustom(param$crown_custom))
}
if (!(identical(param$quad_origin, "centre") || identical(param$quad_origin, "corner") ||
  (is.numeric(param$quad_origin) && length(param$quad_origin) == 2 && 
    all(param$quad_origin >= 0)))) {
  stop("`quad_origin` must be \"centre\", \"corner\", or two numbers >= 0, ",
    "the offset of the quadrat grid from the origin corner of each plot in m, e.g. [10, 10]")
}

if (!is.null(param$als_date)) {
  param$als_date <- as.character(param$als_date)
  if (length(param$als_date) != 1 || is.na(decDate(param$als_date))) {
    stop("`als_date` must be a single date: YYYY-MM-DD, YYYY-MM or YYYY")
  }
}

if (basename(acq_dir) != param$acquisition_id ||
  basename(dirname(acq_dir)) != param$site_id) {
  warning("Location of ", p_file, " does not match `site_id` and `acquisition_id`, ",
    "expected ./sites/", param$site_id, "/", param$acquisition_id, "/<VERSION>/param.yaml")
}

# Import column classes
stem_cols <- read.csv("./templates/stem_cols.csv")
stem_col_class <- setNames(stem_cols$class, stem_cols$column_name)

plot_cols <- read.csv("./templates/plot_cols.csv")
plot_col_class <- setNames(plot_cols$class, plot_cols$column_name)

pt_cols <- read.csv("./templates/pt_cols.csv")
pt_col_class <- setNames(pt_cols$class, pt_cols$column_name)

height_cols <- read.csv("./templates/height_cols.csv")
height_col_class <- setNames(height_cols$class, height_cols$column_name)

wd_cols <- read.csv("./templates/wd_cols.csv")
wd_col_class <- setNames(wd_cols$class, wd_cols$column_name)

taxon_cols <- read.csv("./templates/taxon_cols.csv")
taxon_col_class <- setNames(taxon_cols$class, taxon_cols$column_name)

stem_dup_cols <- read.csv("./templates/stem_dup_cols.csv")
stem_dup_col_class <- setNames(stem_dup_cols$class, stem_dup_cols$column_name)

subplot_cols <- read.csv("./templates/subplot_cols.csv")
subplot_col_class <- setNames(subplot_cols$class, subplot_cols$column_name)

# Create sanitised software version string for filenames
software_version_sanit <- gsub("\\.", "-", param$software_version)

# Directory for all outputs of this software version
ver_dir <- file.path(param$out_dir, software_version_sanit)

# Directory for site reports, with tables and figures written by
# ./12_report.R, and files written by other steps to check raw data and the
# results of each step, e.g. taxonomic names which could not be matched, and
# stems outside plots
report_dir <- file.path(ver_dir, "report")

# Directories for L1, L2, L3 datasets, written by ./11_brm.R
L_list <- c("L1", "L2", "L3")
L_dir_list <- lapply(L_list, function(x) { 
  file.path(ver_dir, x)
})
names(L_dir_list) <- L_list

# Diameter thresholds for large trees, in cm, used in figures and tables for
# site reports in ./05_agb_stem.R and ./12_report.R
large_diam_cm <- c(60, 100)
