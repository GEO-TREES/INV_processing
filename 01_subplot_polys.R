# Create subplot polygons for plots within a site
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-06-18

# Define directories
indir <- "../dat/clean/panama"
outdir <- "../dat/processed/panama"

# Define subplot resolution
subplot_res <- c(50, 50)

# Packages
library(dplyr)
library(sf)

# Source functions
source("./func.R")

# Import data
p <- read_sf(file.path(indir, "polys.gpkg"))
o <- read_sf(file.path(indir, "origins.gpkg"))

# Split plots into subplots
g_corner <- subplotSplit(p, subplot_res, 
  p$origin_dir, p$bearing, name = c("site_id", "plot_id"))

# Write subplots to file
st_write(g_corner, file.path(outdir, "subplot_polys.gpkg"), delete_dsn = TRUE)
