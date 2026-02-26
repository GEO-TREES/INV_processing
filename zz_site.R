# Run the data processing for an individual site
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-11

# DEFINE SITE NAME
# site_id <- "PanamaCanal"

if (!exists("site_id")) {
  stop("site_id must be defined")
}

# DEFINE QUADRAT DIMENSIONS
# quad_dim <- c(50, 50)

if (!exists("quad_dim")) {
  stop("quad_dim must be defined")
}

# Define if raw files located in S3
# opt_s3 <- FALSE

if (!exists("opt_s3")) {
  stop("opt_s3 must be defined")
}

# Load packages
library(sf)
library(BIOMASS)

# Optionally load S3 package
if (opt_s3) {
  library(paws)
}

# Source functions
source("./func.R")

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

# Import site status table
site_status <- read.csv("./dat/site_status.csv")

# Check site name is in sites.csv
stopifnot(site_id %in% site_status$site_id)

# Import column classes
stem_cols <- read.csv("./templates/stem_cols.csv")
stem_col_class <- setNames(stem_cols$class, stem_cols$column_name)

census_cols <- read.csv("./templates/census_cols.csv")
census_col_class <- setNames(census_cols$class, census_cols$column_name)

plot_cols <- read.csv("./templates/plot_cols.csv")
plot_col_class <- setNames(plot_cols$class, plot_cols$column_name)

pt_cols <- read.csv("./templates/pt_cols.csv")
pt_col_class <- setNames(pt_cols$class, pt_cols$column_name)

# Define data directory paths 
if (opt_s3) { 
  # Create local directory for outputs
  local_dir <- "~/PDA_output/sites"
} else {
  local_dir <- "./dat/sites"
}

# Define site data path
site_data <- file.path(local_dir, site_id)
dir.create(site_data, recursive = TRUE)

# Define raw directory
raw_dir <- file.path(site_data, "raw")
dir.create(raw_dir, showWarnings = FALSE)

# Define output directories
out_dirs <- c(
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
  "11_brm"
)

# Optionally wipe existing outputs
# Prompt the user
user_input <- readline(prompt = "Do you want to delete previous outputs? (y/n): ")

if (tolower(trimws(user_input)) %in% c("y", "yes")) {
  message("Deleting previous outputs...")
  
  # Delete output files
  files_all <- list.files(site_data, recursive = TRUE)
  files_out <- files_all[grepl("^[0-9]+_", files_all)]
  files_rem <- files_out[!grepl("wfo_cache.rds", files_out)]
  file.remove(file.path(site_data, files_rem))
  
  # Delete output sub-directories
  site_subdir <- file.path(site_data, out_dirs)
  dirs_all <- list.dirs(site_subdir, recursive = TRUE)
  dirs_sub <- dirs_all[!dirs_all %in% c(site_subdir, site_data)]
  unlink(dirs_sub, recursive = TRUE, expand = FALSE)
  
} else if (!tolower(trimws(user_input)) %in% c("", "n")) {
  stop("User must respond 'y' or 'n'")
}

# Create output directories
for (i in out_dirs) {
  dir.create(file.path(site_data, i), showWarnings = FALSE)
}

# If S3, copy raw data from S3 bucket to local directory
if (opt_s3) {
  # Get object list from S3
  s3_client <- s3(region = "use-west-2")

  # Define S3 bucket where MAAP user directories are located
  bucket <- "maap-ops-workspace"

  # If S3, prompt user for input
  user_input <- readline(prompt = "Enter maap-ops-workspace S3 path to raw data for this site. E.g. johngodlee/GEO-TREES/data/<SITE>: ")
  s3_dir <- user_input

  # Identify files in dir_input
  s3_response <- s3_client$list_objects_v2(Bucket = bucket, Prefix = s3_dir)

  # Collect S3 paths from bucket
  shared_objects <- sapply(s3_response$Contents, "[[", "Key")

  # Retrieve all files
  catch <- lapply(shared_objects, function(x) {
    # Construct new key in destination foldert
    dest_key <- sub(s3_dir, raw_dir, x)

    # Copy files to local directory
    s3_client$download_file(Bucket = bucket, Key = x, Filename = dest_key)

    return(dest_key)
  })
}

# Format raw data
indir <- raw_dir
outdir <- file.path(site_data, "01_fmt")
runFn(file.path("./sites", site_id, "01_fmt.R"))

# Correct taxonomy
outdir <- file.path(site_data, "02_taxa")
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
runFn("./02_taxa.R")

# Split plots into quadrats 
outdir <- file.path(site_data, "03_quad", paste(quad_dim, collapse = "x"))
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
plot_pt <- st_read(file.path(site_data, "01_fmt/plot_pt.gpkg"))
runFn("./03_quad.R")

# Estimate wood density
outdir <- file.path(site_data, "04_wd")
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
stem_taxa <- read.csv(file.path(site_data, "02_taxa/stem_taxa.csv"))
wd <- read.csv("./dat/01_wd/wd.csv")
runFn("./04_wd.R")

# Estimate stem height
outdir <- file.path(site_data, "05_height")
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
plot_pt <- st_read(file.path(site_data, "01_fmt/plot_pt.gpkg"))
runFn("./05_height.R")

# Estimate AGB for every measurement
outdir <- file.path(site_data, "06_agb_stem")
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
stem_wd <- read.csv(file.path(site_data, "04_wd/stem_wd.csv"))
stem_height <- read.csv(file.path(site_data, "05_height/stem_height.csv"))
runFn("./06_agb_stem.R")

# Create master stems table
outdir <- file.path(site_data, "07_stem_summ", paste(quad_dim, collapse = "x"))
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
stem_agb <- read.csv(file.path(site_data, "06_agb_stem/stem_agb.csv"))
stem_height <- read.csv(file.path(site_data, "05_height/stem_height.csv"))
stem_wd <- read.csv(file.path(site_data, "04_wd/stem_wd.csv"))
stem_taxa <- read.csv(file.path(site_data, "02_taxa/stem_taxa.csv"))
stem_pt <- st_read(file.path(site_data, "03_quad", paste(quad_dim, collapse = "x"), "stem_pt.gpkg"))
census <- read.csv(file.path(site_data, "01_fmt/census.csv"), colClasses = census_col_class)
plot <- read.csv(file.path(site_data, "01_fmt/plot.csv"), colClasses = plot_col_class)
runFn("./07_stem_summ.R")

# Filter stem data for quadrat summaries
outdir <- file.path(site_data, "08_stem_fil", paste(quad_dim, collapse = "x"))
dir.create(outdir, showWarnings = FALSE)
stem_summ <- st_read(file.path(site_data, "07_stem_summ", paste(quad_dim, collapse = "x"), "stem_summ.gpkg"))
runFn("./08_stem_fil.R")

# Run AGB Monte-Carlo error propagation
outdir <- file.path(site_data, "09_agb_mc", paste(quad_dim, collapse = "x"))
dir.create(outdir, showWarnings = FALSE)
stem_fil <- read.csv(file.path(site_data, "08_stem_fil", paste(quad_dim, collapse = "x"), "stem_fil.csv"), colClasses = stem_col_class)
plot_pt <- st_read(file.path(site_data, "01_fmt/plot_pt.gpkg"))
stem_pt <- st_read(file.path(site_data, "03_quad", paste(quad_dim, collapse = "x"), "stem_pt.gpkg"))
runFn("./09_agb_mc.R")

# Create master quadrat summary object
outdir <- file.path(site_data, "10_quad_summ", paste(quad_dim, collapse = "x"))
dir.create(outdir, showWarnings = FALSE)
stem_fil <- read.csv(file.path(site_data, "08_stem_fil", paste(quad_dim, collapse = "x"), "stem_fil.csv"), colClasses = c(census_col_class, stem_col_class))
quad_poly <- st_read(file.path(site_data, "03_quad", paste(quad_dim, collapse = "x"), "quad_poly.gpkg"))
quad_agb <- read.csv(file.path(site_data, "09_agb_mc", paste(quad_dim, collapse = "x"), "quad_agb.csv"))
runFn("./10_quad_summ.R")

# Create L2, L3 datasets 
outdir <- file.path(site_data, "11_brm", paste(quad_dim, collapse = "x"))
dir.create(outdir, showWarnings = FALSE)
stem_fil <- read.csv(file.path(site_data, "08_stem_fil", paste(quad_dim, collapse = "x"), "stem_fil.csv"), colClasses = stem_col_class)
quad_summ <- st_read(file.path(site_data, "10_quad_summ", paste(quad_dim, collapse = "x"), "quad_summ.gpkg"))
runFn("./11_brm.R")

# Optionally transfer outputs

# Optionally local files
