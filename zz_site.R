# Run the data processing for an individual site
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-11

# DEFINE SITE NAME
# site_id <- "Panama Canal"

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

# Import data object column classes
stem_cols <- read.csv("./templates/stem_cols.csv")
stem_col_class <- setNames(stem_cols$class, stem_cols$column_name)

pt_cols <- read.csv("./templates/pt_cols.csv")
pt_col_class <- setNames(pt_cols$class, pt_cols$column_name)

poly_cols <- read.csv("./templates/poly_cols.csv")
poly_col_class <- setNames(poly_cols$class, poly_cols$column_name)

# Define site script path
site_script <- paste0("./sites/", site_id)

# Define site data path
site_data <- paste0("./dat/sites/", site_id)

# Create output directories
out_dirs <- c(
  "01_plot",
  "02_stem",
  "03_taxa",
  "04_quad",
  "05_wd",
  "06_height",
  "07_agb",
  "08_stem_summ",
  "09_quad_summ"
)
for (i in out_dirs) dir.create(file.path(site_data, i), showWarnings = FALSE)

# Optionally wipe existing outputs
# files_all <- list.files(site_data, recursive = TRUE)
# files_out <- files_all[grepl("^[0-9]+_", files_all)]
# file.remove(file.path(site_data, files_out))

# Run polygon creation script
runFn(file.path(site_script, "01_plot.R"))

# Run stem formatting script
runFn(file.path(site_script, "02_stem.R"))

# Run taxonomy correction script
outdir <- file.path(site_data, "03_taxa")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"), colClasses = stem_col_class)
runFn("./03_taxa.R")

# Run quadrat splitting script
outdir <- file.path(site_data, "04_quad")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"), colClasses = stem_col_class)
plot_pt <- st_read(file.path(site_data, "01_plot/plot_pt.gpkg"))
runFn("./04_quad.R")

# Run wood density script
outdir <- file.path(site_data, "05_wd")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"), colClasses = stem_col_class)
stem_taxa <- read.csv(file.path(site_data, "03_taxa/stem_taxa.csv"))
runFn("./05_wd.R")

# Run height estimation script
outdir <- file.path(site_data, "06_height")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"), colClasses = stem_col_class)
plot_poly <- st_read(file.path(site_data, "01_plot/plot_poly.gpkg"))
runFn("./06_height.R")

# Run biomass estimation script
outdir <- file.path(site_data, "07_agb")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"), colClasses = stem_col_class)
stem_wd <- read.csv(file.path(site_data, "05_wd/stem_wd.csv"))
stem_height <- read.csv(file.path(site_data, "06_height/stem_height.csv"))
runFn("./07_agb.R")

# Run stem table creation script
outdir <- file.path(site_data, "08_stem_summ")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"), colClasses = stem_col_class)
stem_wd <- read.csv(file.path(site_data, "05_wd/stem_wd.csv"))
stem_height <- read.csv(file.path(site_data, "06_height/stem_height.csv"))
stem_agb <- read.csv(file.path(site_data, "07_agb/stem_agb.csv"))
stem_taxa <- read.csv(file.path(site_data, "03_taxa/stem_taxa.csv"))
stem_pt <- st_read(file.path(site_data, "04_quad/stem_pt.gpkg"))
runFn("./08_stem_summ.R")

# Run quadrat summary script
outdir <- file.path(site_data, "09_quad_summ")
stem_summ <- st_read(file.path(site_data, "08_stem_summ/stem_summ.gpkg"))
quad_poly <- st_read(file.path(site_data, "04_quad/quad_poly.gpkg"))
runFn("./09_quad_summ.R")

