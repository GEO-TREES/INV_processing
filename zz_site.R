# Run the data processing for an individual site
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-03-03

# Load packages
library(dplyr)
library(tidyr)
library(readxl)
library(units)
library(sf)
library(BIOMASS)
library(yaml)
library(rocrateR)

# Source functions
source("./func.R")

# Load YAML file with parameters
# p <- read_yaml("./param.yaml")

# Load YAML file with software version
version <- read_yaml("./version.yaml")

# Merge parameters lists
param <- c(p, version)

# Define parameter names
param_name_vec <- c(
  "site_id",
  "acquisition_id",
  "quad_dim",
  "opt_s3",
  "s3_dir",
  "out_dir",
  "raw_dir",
  "product_version"
)

# Check all parameters present
if (all(sort(names(param)) != sort(param_name_vec))) {
    stop("The following parameters must be named in ./param.yaml: ", 
      paste(param_name_vec, collapse = ", "))
}

# Optionally load S3 package
if (param$opt_s3) {
  library(paws)
}

# Import column classes
stem_cols <- read.csv("./templates/stem_cols.csv")
stem_col_class <- setNames(stem_cols$class, stem_cols$column_name)

taxon_cols <- read.csv("./templates/taxon_cols.csv")
taxon_col_class <- setNames(taxon_cols$class, taxon_cols$column_name)

census_cols <- read.csv("./templates/census_cols.csv")
census_col_class <- setNames(census_cols$class, census_cols$column_name)

plot_cols <- read.csv("./templates/plot_cols.csv")
plot_col_class <- setNames(plot_cols$class, plot_cols$column_name)

pt_cols <- read.csv("./templates/pt_cols.csv")
pt_col_class <- setNames(pt_cols$class, pt_cols$column_name)

# Define output directories
out_dir_vec <- c(
  "01_fmt",
  "02_taxa",
  "03_quad",
  "04_wd",
  "05_height",
  "06_agb_stem",
  "07_stem_summ",
  "08_stem_fil",
  "09_agb_mc",
  "10_quad_summ",
  "L1",
  "L2",
  "L3"
)

# Create sanitised product version string for filenames
product_version_sanit <- gsub("\\.", "-", param$product_version)

# Optionally wipe existing outputs
# Prompt the user
user_input <- readline(prompt = "Do you want to delete previous outputs? (y/n): ")

if (tolower(trimws(user_input)) %in% c("y", "yes")) {
  message("Deleting previous outputs...")
  
  # Delete output files
  files_all <- list.files(param$out_dir, recursive = TRUE)
  files_out <- files_all[grepl("^[0-9]+_", files_all)]
  files_rem <- files_out[!grepl("wfo_cache.rds", files_out)]
  file.remove(file.path(param$out_dir, files_rem))
  
  # Delete output sub-directories
  site_subdir <- file.path(param$out_dir, out_dir_vec, product_version_sanit)
  dirs_all <- list.dirs(site_subdir, recursive = TRUE)
  dirs_sub <- dirs_all[!dirs_all %in% c(site_subdir, param$out_dir)]
  unlink(dirs_sub, recursive = TRUE, expand = FALSE)
  
} else if (!tolower(trimws(user_input)) %in% c("", "n")) {
  stop("User must respond 'y' or 'n'")
}

# Create output directories
for (i in out_dir_vec) {
  dir.create(file.path(param$out_dir, i), showWarnings = FALSE)
}

# If S3, copy raw data from S3 bucket to local directory
if (param$opt_s3) {
  # Get object list from S3
  s3_client <- s3(region = "us-west-2")

  # Define S3 bucket where MAAP user directories are located
  bucket <- "maap-ops-workspace"

  # Identify files in dir_input
  s3_response <- s3_client$list_objects_v2(Bucket = bucket, Prefix = param$s3_dir)

  # Collect S3 paths from bucket
  shared_objects <- sapply(s3_response$Contents, "[[", "Key")

  # Check files detected
  if (length(shared_objects) == 0) { 
    stop("No raw data files detected in S3 bucket")
  }
  
  # Retrieve all files
  for (i in shared_objects) {
    # Construct new key in destination folder
    dest_key <- sub(param$s3_dir, param$raw_dir, i)

    # Copy files to local directory
    s3_client$download_file(Bucket = bucket, Key = i, Filename = dest_key)
  }
}

# Create BIOMASS cache
closeAllConnections()
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
outdir <- file.path(param$out_dir, "01_fmt", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
runFn(file.path("./sites", param$site_id, "01_fmt.R"))

# Correct taxonomy
outdir <- file.path(param$out_dir, "02_taxa", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(param$out_dir, "01_fmt", product_version_sanit, "stem.csv"), 
  colClasses = stem_col_class)
wfo_path <- file.path(outdir, "wfo_cache.rds")
if (file.exists(wfo_path)) { 
  message("WFO cache loaded")
  loadWFOCache(wfo_path) }
runFn("./02_taxa.R")

# Split plots into quadrats 
outdir <- file.path(param$out_dir, "03_quad", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(param$out_dir, "01_fmt", product_version_sanit, "stem.csv"), 
  colClasses = stem_col_class)
plot_pt <- st_read(file.path(param$out_dir, "01_fmt", product_version_sanit, "plot_pt.gpkg"))
runFn("./03_quad.R")

# Estimate wood density
outdir <- file.path(param$out_dir, "04_wd", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(param$out_dir, "01_fmt", product_version_sanit, "stem.csv"), 
  colClasses = stem_col_class)
stem_taxa <- read.csv(file.path(param$out_dir, "02_taxa", product_version_sanit, "stem_taxa.csv"))
wd <- read.csv("./dat/01_wd/wd.csv")
runFn("./04_wd.R")

# Estimate stem height
outdir <- file.path(param$out_dir, "05_height", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(param$out_dir, "01_fmt", product_version_sanit, "stem.csv"), 
  colClasses = stem_col_class)
plot_pt <- st_read(file.path(param$out_dir, "01_fmt", product_version_sanit, "plot_pt.gpkg"))
runFn("./05_height.R")

# Estimate AGB for every measurement
outdir <- file.path(param$out_dir, "06_agb_stem", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(param$out_dir, "01_fmt", product_version_sanit, "stem.csv"), 
  colClasses = stem_col_class)
stem_wd <- read.csv(file.path(param$out_dir, "04_wd", product_version_sanit, "stem_wd.csv"))
stem_height <- read.csv(file.path(param$out_dir, "05_height", product_version_sanit, "stem_height.csv"))
runFn("./06_agb_stem.R")

# Create master stems table
outdir <- file.path(param$out_dir, "07_stem_summ", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(param$out_dir, "01_fmt", product_version_sanit, "stem.csv"), 
  colClasses = stem_col_class)
taxon_path <- file.path(param$out_dir, "01_fmt", product_version_sanit, "taxon.csv")
if (file.exists(taxon_path)) { 
  taxon <- read.csv(taxon_path, colClasses = taxon_col_class)
}
stem_agb <- read.csv(file.path(param$out_dir, "06_agb_stem", product_version_sanit, "stem_agb.csv"))
stem_height <- read.csv(file.path(param$out_dir, "05_height", product_version_sanit, "stem_height.csv"))
stem_wd <- read.csv(file.path(param$out_dir, "04_wd", product_version_sanit, "stem_wd.csv"))
stem_taxa <- read.csv(file.path(param$out_dir, "02_taxa", product_version_sanit, "stem_taxa.csv"))
stem_pt <- st_read(file.path(param$out_dir, "03_quad", product_version_sanit, "stem_pt.gpkg"))
census <- read.csv(file.path(param$out_dir, "01_fmt", product_version_sanit, "census.csv"), 
  colClasses = census_col_class)
plot <- read.csv(file.path(param$out_dir, "01_fmt", product_version_sanit, "plot.csv"), 
  colClasses = plot_col_class)
runFn("./07_stem_summ.R")

# Filter stem data for quadrat summaries
outdir <- file.path(param$out_dir, "08_stem_fil", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
stem_summ <- st_read(file.path(param$out_dir, "07_stem_summ", product_version_sanit, "stem_summ.gpkg"))
runFn("./08_stem_fil.R")

# Run AGB Monte-Carlo error propagation
outdir <- file.path(param$out_dir, "09_agb_mc", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
stem_fil <- read.csv(file.path(param$out_dir, "08_stem_fil", product_version_sanit, "stem_fil.csv"), 
  colClasses = stem_col_class)
plot_pt <- st_read(file.path(param$out_dir, "01_fmt", product_version_sanit, "plot_pt.gpkg"))
stem_pt <- st_read(file.path(param$out_dir, "03_quad", product_version_sanit, "stem_pt.gpkg"))
runFn("./09_agb_mc.R")

# Create master quadrat summary object
outdir <- file.path(param$out_dir, "10_quad_summ", product_version_sanit)
dir.create(outdir, showWarnings = FALSE)
stem_fil <- read.csv(file.path(param$out_dir, "08_stem_fil", product_version_sanit, "stem_fil.csv"), 
  colClasses = c(census_col_class, stem_col_class))
quad_poly <- st_read(file.path(param$out_dir, "03_quad", product_version_sanit, "quad_poly.gpkg"))
quad_agb <- read.csv(file.path(param$out_dir, "09_agb_mc", product_version_sanit, "quad_agb.csv"))
runFn("./10_quad_summ.R")

# Create L1, L2, L3 datasets 
L_list <- c("L1", "L2", "L3")
L_dir_list <- lapply(L_list, function(x) { 
  file.path(param$out_dir, x, product_version_sanit)
})
names(L_dir_list) <- L_list
lapply(L_dir_list, dir.create, recursive = TRUE, showWarnings = FALSE)
stem_fil <- read.csv(file.path(param$out_dir, "08_stem_fil", product_version_sanit, "stem_fil.csv"), 
  colClasses = stem_col_class)
stem_agb_mc <- read.csv(file.path(param$out_dir, "09_agb_mc", product_version_sanit, "stem_agb_mc.csv"))
stem_summ <- st_read(file.path(param$out_dir, "07_stem_summ", product_version_sanit, "stem_summ.gpkg"))
quad_summ <- st_read(file.path(param$out_dir, "10_quad_summ", product_version_sanit, "quad_summ.gpkg"))
runFn("./11_brm.R")

