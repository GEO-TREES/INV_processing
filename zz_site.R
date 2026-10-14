# Run the data processing for one product version of an acquisition at one site
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Remove objects left over from previous runs, e.g. when looping over sites
# in ./zz_site_all.R. Keep `p_file` and any mocked `readline()`
rm(list = setdiff(ls(envir = globalenv()), c("p_file", "readline")), envir = globalenv())

# Load packages
library(dplyr)
library(tidyr)
library(readxl)
library(sf)
library(terra)
library(BIOMASS)
library(yaml)
library(rocrateR)

# Source functions
source("./func.R")

# Load YAML file with parameters for one product version of an acquisition
# p_file <- "./sites/Bicuar/2024-02-12/v1/param.yaml"
p <- yaml::read_yaml(p_file)

# Acquisition directory, ./sites/<SITE>/<ACQUISITION>/, containing 01_fmt.R
# and the WFO cache
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
  "height_method",
  "wd_method",
  "polygon_method",
  "software_version",
  "product_version",
  "rocrate"
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

param_extra <- setdiff(names(param), param_name_vec)
if (length(param_extra) > 0) {
  warning("Unrecognised parameters in ", p_file, ": ", paste(param_extra, collapse = ", "))
}

# Check parameter values
param_choices <- list(
  height_method = c("regional", "field"),
  wd_method = c("regional", "field"),
  polygon_method = c("exact", "affine", "procrustes")
)
for (i in names(param_choices)) {
  if (!param[[i]] %in% param_choices[[i]]) {
    stop("`", i, "` must be one of: ", paste(param_choices[[i]], collapse = ", "))
  }
}

if (length(param$quad_dim) != 2 || !is.numeric(param$quad_dim)) {
  stop("`quad_dim` must be two numbers, e.g. [50, 50]")
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

  # Download all files
  s3Download(s3_client, bucket, prefix = param$s3_dir, dest = param$raw_dir)
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
indir <- param$raw_dir
outdir <- file.path(param$out_dir, software_version_sanit, "01_fmt")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
runFn(file.path(acq_dir, "01_fmt.R"))

# Correct taxonomy
outdir <- file.path(param$out_dir, software_version_sanit, "02_taxa")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
# WFO cache is shared by all product versions of an acquisition
wfo_path <- file.path(acq_dir, "wfo_cache.rds")
if (!file.exists(wfo_path) && file.exists(file.path(outdir, "wfo_cache.rds"))) {
  # Use cache from previous outputs, if any
  file.copy(file.path(outdir, "wfo_cache.rds"), wfo_path)
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
runFn("./02_taxa.R")

# Estimate wood density
outdir <- file.path(param$out_dir, software_version_sanit, "03_wd")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
taxa <- read.csv(file.path(param$out_dir, software_version_sanit, "02_taxa", "stem_taxa.csv"))
if (param$wd_method == "field") {
  wd <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "wd.csv"), colClasses = wd_col_class)
}
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
record_fil <- readLines(file.path(param$out_dir, software_version_sanit, "06_record_fil", "record_fil.txt"))
runFn("./07_quad.R")

# Run AGB Monte-Carlo error propagation
outdir <- file.path(param$out_dir, software_version_sanit, "08_agb_mc")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
stem_wd <- read.csv(file.path(param$out_dir, software_version_sanit, "03_wd", "stem_wd.csv"))
stem_height <- read.csv(file.path(param$out_dir, software_version_sanit, "04_height", "stem_height.csv"))
stem_agb <- read.csv(file.path(param$out_dir, software_version_sanit, "05_agb_stem", "stem_agb.csv"))
record_fil <- readLines(file.path(param$out_dir, software_version_sanit, "06_record_fil", "record_fil.txt"))
quad_pt <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "quad_pt.gpkg"))
stem_pt <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "stem_pt.gpkg"))
if (param$height_method == "regional") { 
  plot_cent <- read.csv(file.path(param$out_dir, software_version_sanit, "04_height", "plot_cent.csv"), colClasses = c(plot_id = "character"))
}
if (param$height_method == "field") { 
  height_mod <- readRDS(file.path(param$out_dir, software_version_sanit, "04_height", "height_mod.rds"))
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

