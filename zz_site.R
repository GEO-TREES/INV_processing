# Run the data processing for an individual site
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-11

# DEFINE SITE NAME
# site_id <- "Panama_Canal"

# DEFINE QUADRAT DIMENSIONS
# quad_dim <- c(50, 50)

# Load packages
library(sf)
library(BIOMASS)

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

# Define site script path
site_script <- paste0("./sites/", site_id)

# Define site data path
site_data <- paste0("./dat/sites/", site_id)

# Create output directories
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
for (i in out_dirs) dir.create(file.path(site_data, i), showWarnings = FALSE)

# Optionally wipe existing outputs
# files_all <- list.files(site_data, recursive = TRUE)
# files_out <- files_all[grepl("^[0-9]+_", files_all)]
# file.remove(file.path(site_data, files_out))

# Format raw data
runFn(file.path(site_script, "01_fmt.R"))

# Correct taxonomy
outdir <- file.path(site_data, "02_taxa")
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
runFn("./02_taxa.R")

# Split plots into quadrats 
outdir <- file.path(site_data, "03_quad", paste(quad_dim, collapse = "_"))
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
plot_pt <- st_read(file.path(site_data, "01_fmt/plot_pt.gpkg"))
runFn("./03_quad.R")

# Estimate wood density
outdir <- file.path(site_data, "04_wd")
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
stem_taxa <- read.csv(file.path(site_data, "02_taxa/stem_taxa.csv"))
runFn("./04_wd.R")

# Estimate stem height
outdir <- file.path(site_data, "05_height")
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
plot_poly <- st_read(file.path(site_data, "01_fmt/plot_poly.gpkg"))
runFn("./05_height.R")

# Estimate AGB for every measurement
outdir <- file.path(site_data, "06_agb_stem")
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
stem_wd <- read.csv(file.path(site_data, "04_wd/stem_wd.csv"))
stem_height <- read.csv(file.path(site_data, "05_height/stem_height.csv"))
runFn("./06_agb_stem.R")

# Create master stems table
outdir <- file.path(site_data, "07_stem_summ", paste(quad_dim, collapse = "_"))
dir.create(outdir, showWarnings = FALSE)
stem <- read.csv(file.path(site_data, "01_fmt/stem.csv"), colClasses = stem_col_class)
stem_agb <- read.csv(file.path(site_data, "06_agb_stem/stem_agb.csv"))
stem_height <- read.csv(file.path(site_data, "05_height/stem_height.csv"))
stem_wd <- read.csv(file.path(site_data, "04_wd/stem_wd.csv"))
stem_taxa <- read.csv(file.path(site_data, "02_taxa/stem_taxa.csv"))
stem_pt <- st_read(file.path(site_data, "03_quad", paste(quad_dim, collapse = "_"), "stem_pt.gpkg"))
census <- read.csv(file.path(site_data, "01_fmt/census.csv"), colClasses = census_col_class)
runFn("./07_stem_summ.R")

# Filter stem data for quadrat summaries
outdir <- file.path(site_data, "08_stem_fil", paste(quad_dim, collapse = "_"))
dir.create(outdir, showWarnings = FALSE)
stem_summ <- st_read(file.path(site_data, "07_stem_summ", paste(quad_dim, collapse = "_"), "stem_summ.gpkg"))
runFn("./08_stem_fil.R")

# Run AGB Monte-Carlo error propagation
outdir <- file.path(site_data, "09_agb_mc", paste(quad_dim, collapse = "_"))
dir.create(outdir, showWarnings = FALSE)
stem_fil <- read.csv(file.path(site_data, "08_stem_fil", paste(quad_dim, collapse = "_"), "stem_fil.csv"), colClasses = stem_col_class)
plot_poly <- st_read(file.path(site_data, "01_fmt/plot_poly.gpkg"))
stem_pt <- st_read(file.path(site_data, "03_quad", paste(quad_dim, collapse = "_"), "stem_pt.gpkg"))
runFn("./09_agb_mc.R")

# Create master quadrat summary object
outdir <- file.path(site_data, "10_quad_summ", paste(quad_dim, collapse = "_"))
dir.create(outdir, showWarnings = FALSE)
stem_fil <- read.csv(file.path(site_data, "08_stem_fil", paste(quad_dim, collapse = "_"), "stem_fil.csv"), colClasses = stem_col_class)
quad_poly <- st_read(file.path(site_data, "03_quad", paste(quad_dim, collapse = "_"), "quad_poly.gpkg"))
quad_agb <- read.csv(file.path(site_data, "09_agb_mc", paste(quad_dim, collapse = "_"), "quad_agb.csv"))
runFn("./10_quad_summ.R")

# Create L2, L3 datasets 
outdir <- file.path(site_data, "11_brm", paste(quad_dim, collapse = "_"))
dir.create(outdir, showWarnings = FALSE)
stem_fil <- read.csv(file.path(site_data, "08_stem_fil", paste(quad_dim, collapse = "_"), "stem_fil.csv"), colClasses = stem_col_class)
quad_summ <- st_read(file.path(site_data, "10_quad_summ", paste(quad_dim, collapse = "_"), "quad_summ.gpkg"))
runFn("./11_brm.R")
