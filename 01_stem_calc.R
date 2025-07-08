# Estimate stem-level above-ground woody biomass from tree inventory data
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-06-18

# Define site ID
BRM_site <- "Panama Canal"

# Define directories
indir <- file.path("../../../dat/raw", BRM_site)
outdir <- file.path("../../../dat/clean", BRM_site)

# Packages
library(dplyr)
library(sf)
library(BIOMASS)

# Source functions
source("./func.R")

# Import stem data
s <- read.csv(file.path(indir, "stems.csv"))

# Import plot polygons
p <- st_read(file.path(indir, "polys.gpkg"))

# TODO: Move code below here to individual files when methods developed
# Get wood density 
s$wd <- getWoodDensity(
  genus = s$genus, 
  species = s$species,
  stand = s$Plot_name,
  family = s$family,
  region = "SouthAmericaTrop")$meanWD

# Add plot mean coordinates to stem data
p_cent <- p %>% 
  st_centroid(.) %>% 
  st_transform(., 4326) %>% 
  bind_cols(., st_coordinates(.)) %>% 
  dplyr::select(
    Plot_name,
    plot_centroid_x = X,
    plot_centroid_y = Y) %>% 
  st_drop_geometry()

s_c <- s %>% 
  left_join(., p_cent, by = "Plot_name")

# Estimate stem biomass (Mg)
s_c$agb <- computeAGB(
  D = s_c$diam,
  WD = s_c$wd,
  H = NULL,
  coord = st_drop_geometry(s_c[,c("plot_centroid_x", "plot_centroid_y")]),
  Dlim = 5)

# Calculate basal area (m^2)
s_c$ba <- pi * (s_c$diam / 2)^2 / 10000 

# Write summarised data to file
write.csv(s_c, file.path(indir, "stems_calc.csv"), row.names = FALSE)
