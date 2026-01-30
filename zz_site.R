# Run the data processing for an individual site
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-08-11

# DEFINE SITE NAME
site_name <- "Bicuar"

# Load packages
library(sf)
library(BIOMASS)

# Source functions
source("./func.R")

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

# Define site script path
site_script <- paste0("./sites/", site_name)

# Define site data path
site_data <- paste0("./dat/sites/", site_name)

# Create output directories
out_dirs <- c(
  "01_plot",
  "02_stem",
  "03_taxa",
  "04_sub",
  "05_wd",
  "06_height",
  "07_agb",
  "08_stem_summ",
  "09_sub_summ"
)
for (i in out_dirs) dir.create(file.path(site_data, i))

# Optionally wipe existing outputs
# files_all <- list.files(site_data, recursive = TRUE)
# files_out <- files_all[grepl("^[0-9]+_", files_all)]
# file.remove(file.path(site_data, files_out))

# Run polygon creation script
run_fn(file.path(site_script, "01_plot.R"))

# Run stem formatting script
run_fn(file.path(site_script, "02_stem.R"))

# Run taxonomy correction script
run_fn(file.path(site_script, "03_taxa.R"))

# Run subplot splitting script
outdir <- file.path(site_data, "04_sub")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"))
plot_pt <- st_read(file.path(site_data, "01_plot/plot_pt.gpkg"))
run_fn("./04_sub.R")

# Run wood density script
outdir <- file.path(site_data, "05_wd")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"))
stem_taxa <- read.csv(file.path(site_data, "03_taxa/stem_taxa.csv"))
run_fn("./05_wd.R")

# Run height estimation script
outdir <- file.path(site_data, "06_height")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"))
plot_poly <- st_read(file.path(site_data, "01_plot/plot_poly.gpkg"))
run_fn("./06_height.R")

# Run biomass estimation script
outdir <- file.path(site_data, "07_agb")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"))
stem_wd <- read.csv(file.path(site_data, "05_wd/stem_wd.csv"))
stem_height <- read.csv(file.path(site_data, "06_height/stem_height.csv"))
run_fn("./07_agb.R")

# Run stem table creation script
outdir <- file.path(site_data, "08_stem_summ")
stem <- read.csv(file.path(site_data, "02_stem/stem.csv"))
stem_wd <- read.csv(file.path(site_data, "05_wd/stem_wd.csv"))
stem_height <- read.csv(file.path(site_data, "06_height/stem_height.csv"))
stem_agb <- read.csv(file.path(site_data, "07_agb/stem_agb.csv"))
stem_taxa <- read.csv(file.path(site_data, "03_taxa/stem_taxa.csv"))
stem_pt <- st_read(file.path(site_data, "04_sub/stem_pt.gpkg"))
run_fn("./08_stem_summ.R")

# Run subplot summary script
outdir <- file.path(site_data, "09_sub_summ")
stem_all <- st_read(file.path(site_data, "08_stem_summ/stem_summ.gpkg"))
sub_poly <- st_read(file.path(site_data, "04_sub/sub_poly.gpkg"))
run_fn("./09_sub_summ.R")

