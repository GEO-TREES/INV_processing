# Run the data processing for one product version of an acquisition at one site
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-21

# Remove objects left over from previous runs, e.g. when looping over sites
# in ./zz_site_all.R. Keep `p_file` and any mocked `readline()`
rm(list = setdiff(
    ls(envir = globalenv()), 
    c("p_file", "readline")
    ), 
  envir = globalenv())

# Load packages
library(dplyr)
library(tidyr)
library(readxl)
library(sf)
library(terra)
library(BIOMASS)
library(yaml)
library(rocrateR)
library(ggplot2)

# Source functions
source("./func.R")

# Load YAML file with parameters for one product version of an acquisition
# p_file <- "./sites/Paracou/2019/v1/param.yaml"
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
param_optional_vec <- c(
  "als_date",
  "quad_origin"
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
  height_method = c("regional", "field"),
  wd_method = c("regional", "field"),
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

# Define output directories
out_dir_vec <- c(
  "01_fmt",
  "02_taxa",
  "07_quad",
  "03_wd",
  "04_height",
  "05_agb_stem",
  "06_record_fil",
  "08_agb_mc",
  "09_stem_summ",
  "10_quad_summ",
  "report",
  "L1",
  "L2",
  "L3"
)

# Create sanitised software version string for filenames
software_version_sanit <- gsub("\\.", "-", param$software_version)

# Optionally wipe existing outputs
# Prompt the user
user_input <- readline(prompt = "Do you want to delete previous outputs? (y/n): ")

if (tolower(trimws(user_input)) %in% c("y", "yes")) {
  message("Deleting previous outputs...")
  
  # Delete all outputs for this software version, except the WFO cache which
  # stores choices made during interactive taxonomic name matching
  ver_dir <- file.path(param$out_dir, software_version_sanit)
  files_rem <- list.files(ver_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  files_rem <- files_rem[basename(files_rem) != "wfo_cache.rds"]
  invisible(file.remove(files_rem))

  # Delete empty sub-directories, deepest first
  dirs_rem <- setdiff(list.dirs(ver_dir, recursive = TRUE), ver_dir)
  for (i in dirs_rem[order(nchar(dirs_rem), decreasing = TRUE)]) {
    if (length(list.files(i, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(i, recursive = TRUE)
    }
  }
  
} else if (!tolower(trimws(user_input)) %in% c("", "n")) {
  stop("User must respond 'y' or 'n'")
}

# Directory for site reports, with tables and figures written by
# ./12_report.R, and files written by other steps to check raw data and the
# results of each step, e.g. taxonomic names which could not be matched, and
# stems outside plots
report_dir <- file.path(param$out_dir, software_version_sanit, "report")

# Diameter thresholds for large trees, in cm, used in figures and tables for
# site reports in ./05_agb_stem.R and ./12_report.R
large_diam_cm <- c(60, 100)

# Create output directories
for (i in out_dir_vec) {
  dir.create(file.path(param$out_dir, software_version_sanit, i), 
    showWarnings = FALSE, recursive = TRUE)
}

# If S3, copy raw data from S3 bucket to local directory
if (param$opt_s3) {
  # Define S3 bucket where MAAP user directories are located
  s3_client <- paws::s3(config = list(region = "us-west-2"))
  bucket <- "maap-ops-workspace"

  # Download all files for this version of the raw data
  s3Download(s3_client, bucket, prefix = file.path(param$s3_dir, param$raw_version),
    dest = file.path(param$raw_dir, param$raw_version))
}

# Create BIOMASS cache
BIOMASS_cache <- "./dat/BIOMASS/cache"
BIOMASS::createCache(BIOMASS_cache)
BIOMASS_files <- c(
  "CWD.bil",
  "bio4.bil",
  "bio15.bil",
  "E.bil"
)
for (i in BIOMASS_files) BIOMASS::cacheManager(i)

# Format raw data
# Raw data are versioned, in `raw_dir`/`raw_version`, so that outputs can be
# reproduced after raw data are corrected
indir <- file.path(param$raw_dir, param$raw_version)
if (!dir.exists(indir)) {
  stop("Raw data directory not found: ", indir)
}
outdir <- file.path(param$out_dir, software_version_sanit, "01_fmt")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
runFn(file.path(acq_dir, "01_fmt.R"))

# Correct taxonomy
outdir <- file.path(param$out_dir, software_version_sanit, "02_taxa")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
# WFO cache is shared by all product versions of an acquisition, and is
# stored with the raw data, in the directory containing `raw_dir`
wfo_path <- file.path(dirname(param$raw_dir), "wfo_cache.rds")
if (!file.exists(wfo_path)) {
  # Use cache from a previous location, if any
  wfo_old <- c(file.path(acq_dir, "wfo_cache.rds"), file.path(outdir, "wfo_cache.rds"))
  wfo_old <- wfo_old[file.exists(wfo_old)]
  if (length(wfo_old) > 0) {
    file.copy(wfo_old[1], wfo_path)
  }
}
loadWFOCache(wfo_path) 
if (file.exists(file.path(param$out_dir, software_version_sanit, "01_fmt", "wd.csv"))) {
  wd <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "wd.csv"), colClasses = wd_col_class)
} else {
  wd <- NULL
}
if (file.exists(file.path(param$out_dir, software_version_sanit, "01_fmt", "height.csv"))) {
  height <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "height.csv"), colClasses = height_col_class)
} else {
  height <- NULL
}
if (file.exists(file.path(param$out_dir, software_version_sanit, "01_fmt", "taxon.csv"))) {
  taxon <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "taxon.csv"), colClasses = taxon_col_class)
} else {
  taxon <- NULL
}
runFn("./02_taxa.R")

# Estimate wood density
outdir <- file.path(param$out_dir, software_version_sanit, "03_wd")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
taxa <- read.csv(file.path(param$out_dir, software_version_sanit, "02_taxa", "stem_taxa.csv"))
if (param$wd_method == "field") {
  wd <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "wd.csv"), colClasses = wd_col_class)
}
plot <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot.csv"), colClasses = plot_col_class)
runFn("./03_wd.R")

# Estimate stem height
outdir <- file.path(param$out_dir, software_version_sanit, "04_height")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
if (param$height_method == "regional") { 
  plot_pt <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot_pt.csv"), colClasses = pt_col_class) 
}
if (param$height_method == "field") {
  height <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "height.csv"), colClasses = height_col_class)
}
plot <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot.csv"), colClasses = plot_col_class)
height <- readOpt(file.path(param$out_dir, software_version_sanit, "01_fmt", "height.csv"), colClasses = height_col_class)
runFn("./04_height.R")

# Estimate AGB for every measurement
outdir <- file.path(param$out_dir, software_version_sanit, "05_agb_stem")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
stem_wd <- read.csv(file.path(param$out_dir, software_version_sanit, "03_wd", "stem_wd.csv"))
stem_height <- read.csv(file.path(param$out_dir, software_version_sanit, "04_height", "stem_height.csv"))
plot <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot.csv"), colClasses = plot_col_class)
if (param$height_method == "regional") { 
  plot_cent <- read.csv(file.path(param$out_dir, software_version_sanit, "04_height", "plot_cent.csv"), colClasses = c(plot_id = "character"))
}
if (param$height_method == "field") {
  height_mod <- readRDS(file.path(param$out_dir, software_version_sanit, "04_height", "height_mod.rds"))
}
runFn("./05_agb_stem.R")

# Filter stem data for quadrat summaries
outdir <- file.path(param$out_dir, software_version_sanit, "06_record_fil")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
plot <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot.csv"), colClasses = plot_col_class)
runFn("./06_record_fil.R")

# Split plots into quadrats 
outdir <- file.path(param$out_dir, software_version_sanit, "07_quad")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
plot_pt <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot_pt.csv"), colClasses = pt_col_class)
record_agbd <- readLines(file.path(param$out_dir, software_version_sanit, "06_record_fil", "record_agbd.txt"))
runFn("./07_quad.R")

# Run AGB Monte-Carlo error propagation
outdir <- file.path(param$out_dir, software_version_sanit, "08_agb_mc")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
stem_wd <- read.csv(file.path(param$out_dir, software_version_sanit, "03_wd", "stem_wd.csv"))
stem_height <- read.csv(file.path(param$out_dir, software_version_sanit, "04_height", "stem_height.csv"))
stem_agb <- read.csv(file.path(param$out_dir, software_version_sanit, "05_agb_stem", "stem_agb.csv"))
record_fil <- readLines(file.path(param$out_dir, software_version_sanit, "06_record_fil", "record_fil.txt"))
record_agbd <- readLines(file.path(param$out_dir, software_version_sanit, "06_record_fil", "record_agbd.txt"))
quad_pt <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "quad_pt.gpkg"))
stem_pt <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "stem_pt.gpkg"))
if (param$height_method == "regional") { 
  plot_cent <- read.csv(file.path(param$out_dir, software_version_sanit, "04_height", "plot_cent.csv"), colClasses = c(plot_id = "character"))
}
if (param$height_method == "field") { 
  height_mod <- readRDS(file.path(param$out_dir, software_version_sanit, "04_height", "height_mod.rds"))
}
if (identical(param$diam_error, "field")) {
  stem_dup_file <- file.path(param$out_dir, software_version_sanit, "01_fmt", "stem_dup.csv")
  if (!file.exists(stem_dup_file)) {
    stop("`diam_error` is \"field\", but no repeat diameter measurements found: ", stem_dup_file)
  }
  stem_dup <- read.csv(stem_dup_file, colClasses = stem_dup_col_class)
}
runFn("./08_agb_mc.R")

# Create master stem summary object
outdir <- file.path(param$out_dir, software_version_sanit, "09_stem_summ")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
taxa <- read.csv(file.path(param$out_dir, software_version_sanit, "02_taxa", "stem_taxa.csv"))
stem_wd <- read.csv(file.path(param$out_dir, software_version_sanit, "03_wd", "stem_wd.csv"))
stem_height <- read.csv(file.path(param$out_dir, software_version_sanit, "04_height", "stem_height.csv"))
stem_agb <- read.csv(file.path(param$out_dir, software_version_sanit, "05_agb_stem", "stem_agb.csv"))
record_fil <- readLines(file.path(param$out_dir, software_version_sanit, "06_record_fil", "record_fil.txt"))
record_agbd <- readLines(file.path(param$out_dir, software_version_sanit, "06_record_fil", "record_agbd.txt"))
stem_pt <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "stem_pt.gpkg"))
stem_agb_mc <- read.csv(file.path(param$out_dir, software_version_sanit, "08_agb_mc", "stem_agb_mc.csv"))
runFn("./09_stem_summ.R")

# Create master quadrat summary object
outdir <- file.path(param$out_dir, software_version_sanit, "10_quad_summ")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
quad_poly <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "quad_poly.gpkg"))
quad_agb <- read.csv(file.path(param$out_dir, software_version_sanit, "08_agb_mc", "quad_agb.csv"), colClasses = c(plot_id = "character", quadrat_id = "character"))
stem_summ <- st_read(file.path(param$out_dir, software_version_sanit, "09_stem_summ", "stem_summ.gpkg"))
runFn("./10_quad_summ.R")

# Create L1, L2, L3 datasets 
L_list <- c("L1", "L2", "L3")
L_dir_list <- lapply(L_list, function(x) { 
  file.path(param$out_dir, software_version_sanit, x)
})
names(L_dir_list) <- L_list
lapply(L_dir_list, dir.create, recursive = TRUE, showWarnings = FALSE)
plot <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot.csv"), colClasses = plot_col_class)  # L1_plot
plot_pt <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot_pt.csv"), colClasses = pt_col_class)  # L1_pt
plot_poly <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "plot_poly.gpkg"))  # L2_poly
stem_summ <- st_read(file.path(param$out_dir, software_version_sanit, "09_stem_summ", "stem_summ.gpkg"))  # L1_stem, L2_stem
quad_summ <- st_read(file.path(param$out_dir, software_version_sanit, "10_quad_summ", "quad_summ.gpkg"))  # L3_quad
runFn("./11_brm.R")

# Summarise outputs and processing choices for site reports
outdir <- report_dir
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
ver_dir <- file.path(param$out_dir, software_version_sanit)
id_class <- c(plot_id = "character", quadrat_id = "character")
plot <- read.csv(file.path(ver_dir, "01_fmt", "plot.csv"), colClasses = plot_col_class)
plot_pt <- read.csv(file.path(ver_dir, "01_fmt", "plot_pt.csv"), colClasses = pt_col_class)
plot_poly <- st_read(file.path(ver_dir, "07_quad", "plot_poly.gpkg"))
stem_summ <- st_read(file.path(ver_dir, "09_stem_summ", "stem_summ.gpkg"))
quad_summ <- st_read(file.path(ver_dir, "10_quad_summ", "quad_summ.gpkg"))
quad_agb_simu <- read.csv(file.path(ver_dir, "08_agb_mc", "quad_agb_mc.csv"), colClasses = id_class)
plot_agb <- read.csv(file.path(ver_dir, "08_agb_mc", "plot_agb.csv"), colClasses = c(plot_id = "character"))
poly_check <- read.csv(file.path(ver_dir, "07_quad", "plot_poly_check.csv"), colClasses = c(plot_id = "character"))
record_status <- read.csv(file.path(ver_dir, "06_record_fil", "record_status.csv"))
taxa_outcome <- read.csv(file.path(ver_dir, "02_taxa", "taxa_outcome.csv"))
height_mod_comp <- readOpt(file.path(ver_dir, "04_height", "height_mod_comp.csv"))
diam_error_fit <- readOpt(file.path(ver_dir, "08_agb_mc", "diam_error.csv"))
diam_dup <- readOpt(file.path(report_dir, "diam_dup.csv"))
stem_outside <- readOpt(file.path(report_dir, "stem_outside.csv"), colClasses = c(plot_id = "character"))
tree_unmapped <- readOpt(file.path(report_dir, "tree_unmapped.csv"), colClasses = c(plot_id = "character"))
height <- readOpt(file.path(ver_dir, "01_fmt", "height.csv"), colClasses = height_col_class)
wd <- readOpt(file.path(ver_dir, "01_fmt", "wd.csv"), colClasses = wd_col_class)
taxon <- readOpt(file.path(ver_dir, "01_fmt", "taxon.csv"), colClasses = taxon_col_class)
runFn("./12_report.R")

# List files written for site reports, and to check the results of each step
message("Files for site reports and to check results, in ", report_dir, ":\n  ",
  paste(list.files(report_dir), collapse = "\n  "))

