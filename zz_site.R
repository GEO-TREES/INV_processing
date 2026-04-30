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
library(brms)
library(yaml)
library(rocrateR)

# Source functions
source("./func.R")

# Load YAML file with parameters for 
# p <- yaml::read_yaml("./sites/<SITE>/<ACQUISITION>/<PRODUCTVERSION>/param.yaml")

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
  "software_version",
  "product_version"
)

# Check all parameters present
if (all(sort(names(param)) != sort(param_name_vec))) {
    stop("The following parameters must be named in ./param.yaml: ", 
      paste(param_name_vec, collapse = ", "))
}
# sort(names(param))
# sort(param_name_vec)

# Optionally load AWS S3 package
if (param$opt_s3) {
  library(paws)
}

# Import column classes
stem_cols <- read.csv("./templates/stem_cols.csv")
stem_col_class <- setNames(stem_cols$class, stem_cols$column_name)

taxon_cols <- read.csv("./templates/taxon_cols.csv")
taxon_col_class <- setNames(taxon_cols$class, taxon_cols$column_name)

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
  
  # Delete output files
  files_all <- list.files(param$out_dir, recursive = TRUE)
  files_out <- files_all[grepl(
    paste0("^", software_version_sanit, "/", "[0-9]+_|^L[1-3]"), files_all)]
  files_rem <- files_out[!grepl("wfo_cache.rds", files_out)]
  file.remove(file.path(param$out_dir, files_rem))
  
  # Delete output sub-directories
  site_subdir <- file.path(param$out_dir, out_dir_vec, software_version_sanit)
  dirs_all <- list.dirs(site_subdir, recursive = TRUE)
  dirs_sub <- dirs_all[!dirs_all %in% c(site_subdir, param$out_dir)]
  unlink(dirs_sub, recursive = TRUE, expand = FALSE)
  
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
outdir <- file.path(param$out_dir, software_version_sanit, "01_fmt")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
runFn(file.path("./sites", param$site_id, param$acquisition_id, "01_fmt.R"))

# Correct taxonomy
outdir <- file.path(param$out_dir, software_version_sanit, "02_taxa")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
wfo_path <- file.path(outdir, "wfo_cache.rds")
if (file.exists(wfo_path)) { 
  message("WFO cache loaded")
  loadWFOCache(wfo_path) 
}
if (file.exists(file.path(param$out_dir, software_version_sanit, "01_fmt", "wd.csv"))) { 
  wd <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "wd.csv"))
} else { 
  wd <- NULL
}
if (file.exists(file.path(param$out_dir, software_version_sanit, "01_fmt", "height.csv"))) { 
  height <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "height.csv"))
} else { 
  height <- NULL
}
if (file.exists(file.path(param$out_dir, software_version_sanit, "01_fmt", "taxon.csv"))) {
  taxon <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "taxon.csv"))
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
  wd <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "wd.csv"))
}
runFn("./03_wd.R")

# Estimate stem height
outdir <- file.path(param$out_dir, software_version_sanit, "04_height")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
if (param$height_method == "regional") { 
  plot_pt <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot_pt.csv"))
}
if (param$height_method == "field") { 
  height <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "height.csv"))
}
runFn("./04_height.R")

# Estimate AGB for every measurement
outdir <- file.path(param$out_dir, software_version_sanit, "05_agb_stem")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
stem_wd <- read.csv(file.path(param$out_dir, software_version_sanit, "03_wd", "stem_wd.csv"))
stem_height <- read.csv(file.path(param$out_dir, software_version_sanit, "04_height", "stem_height.csv"))
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
plot_pt <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot_pt.csv"))
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
if (param$height_method == "field") { 
  height_mod <- readRDS(file.path(param$out_dir, software_version_sanit, "04_height", "height_mod.rds"))
}
runFn("./08_agb_mc.R")

# Create master stem summary object
outdir <- file.path(param$out_dir, software_version_sanit, "09_stem_summ")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
stem <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "stem.csv"), colClasses = stem_col_class)
stem_taxa <- read.csv(file.path(param$out_dir, software_version_sanit, "02_taxa", "stem_taxa.csv"))
stem_wd <- read.csv(file.path(param$out_dir, software_version_sanit, "03_wd", "stem_wd.csv"))
stem_height <- read.csv(file.path(param$out_dir, software_version_sanit, "04_height", "stem_height.csv"))
stem_agb <- read.csv(file.path(param$out_dir, software_version_sanit, "05_agb_stem", "stem_agb.csv"))
record_fil <- readLines(file.path(param$out_dir, software_version_sanit, "06_record_fil", "record_fil.txt"))
quad_poly <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "quad_poly.gpkg"))
stem_agb_mc <- read.csv(file.path(param$out_dir, software_version_sanit, "08_agb_mc", "stem_agb_mc.csv"))
runFn("./09_stem_summ.R")

# Create master quadrat summary object
outdir <- file.path(param$out_dir, software_version_sanit, "10_quad_summ")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
quad_poly <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "quad_poly.gpkg"))
quad_agb <- read.csv(file.path(param$out_dir, software_version_sanit, "08_agb_mc", "quad_agb.csv"))
stem_summ <- st_read(file.path(param$out_dir, software_version_sanit, "09_stem_summ", "stem_summ.gpkg"))
runFn("./10_quad_summ.R")

# Create L1, L2, L3 datasets 
L_list <- c("L1", "L2", "L3")
L_dir_list <- lapply(L_list, function(x) { 
  file.path(param$out_dir, software_version_sanit, x)
})
names(L_dir_list) <- L_list
lapply(L_dir_list, dir.create, recursive = TRUE, showWarnings = FALSE)
plot <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot.csv"))  # L1_plot
plot_pt <- read.csv(file.path(param$out_dir, software_version_sanit, "01_fmt", "plot_pt.csv"))  # L1_pt
plot_poly <- st_read(file.path(param$out_dir, software_version_sanit, "07_quad", "plot_poly.gpkg"))  # L2_poly
stem_summ <- st_read(file.path(param$out_dir, software_version_sanit, "09_stem_summ", "stem_summ.gpkg"))  # L1_stem, L2_stem
quad_summ <- st_read(file.path(param$out_dir, software_version_sanit, "10_quad_summ", "quad_summ.gpkg"))  # L3_quad
runFn("./11_brm.R")

