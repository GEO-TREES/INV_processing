# Run the data processing for an individual site
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-08-11

# DEFINE SITE NAME
site_name <- "Bicuar"

# Load packages
library(sf)

# Source functions
source("./func.R")

# Define site script path
site_script <- paste0("./sites/", site_name)

# Run polygon creation script
run_fn(file.path(site_script, "01_polys.R"))

# Run stem formatting script
run_fn(file.path(site_script, "02_stem_fmt.R"))

# Run taxonomy correction script
run_fn(file.path(site_script, "03_taxa.R"))

# Define site data path
site_data <- paste0("./dat/sites/", site_name)

# Run subplot splitting script
outdir <- file.path(site_data, "04_subplots")
stems <- read.csv(file.path(site_data, "02_stem_fmt/stems.csv"))
pts <- st_read(file.path(site_data, "01_polys/pts.gpkg"))
run_fn("./04_subplots.R")

# Run wood density script
outdir <- file.path(site_data, "05_wd")
stems <- read.csv(file.path(site_data, "02_stem_fmt/stems.csv"))
taxa <- read.csv(file.path(site_data, "03_taxa/taxa.csv"))
run_fn("./05_wd.R")

# Run height estimation script
outdir <- file.path(site_data, "06_height")
stems <- read.csv(file.path(site_data, "02_stem_fmt/stems.csv"))
polys <- st_read(file.path(site_data, "01_polys/polys.gpkg"))
run_fn("./06_height.R")

# Run biomass estimation script
outdir <- file.path(site_data, "07_biomass")
stems <- read.csv(file.path(site_data, "02_stem_fmt/stems.csv"))
wd <- read.csv(file.path(site_data, "05_wd/wd.csv"))
height <- read.csv(file.path(site_data, "06_height/height.csv"))
run_fn("./07_biomass.R")

# Run stem table creation script
outdir <- file.path(site_data, "08_stem_out")
stems <- read.csv(file.path(site_data, "02_stem_fmt/stems.csv"))
wd <- read.csv(file.path(site_data, "05_wd/wd.csv"))
height <- read.csv(file.path(site_data, "06_height/height.csv"))
biomass <- read.csv(file.path(site_data, "07_biomass/biomass.csv"))
taxa <- read.csv(file.path(site_data, "03_taxa/taxa.csv"))
stems_coords <- st_read(file.path(site_data, "04_subplots/stems_coords.gpkg"))
run_fn("./08_stem_out.R")

# Run subplot summary script
outdir <- file.path(site_data, "09_sub_summ")
stems_all <- st_read(file.path(site_data, "08_stem_out/stems_all.gpkg"))
polys_sub <- st_read(file.path(site_data, "04_subplots/polys_sub.gpkg"))
run_fn("./09_sub_summ.R")

