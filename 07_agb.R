# Estimate stem-level above-ground woody biomass from tree inventory data
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-06-18

# Define directories
# outdir <- "./dat/sites/Panama Canal/07_agb"

# Packages
library(dplyr)
library(BIOMASS)

# Source functions
source("./func.R")

# Import data
# stem <- read.csv("./dat/sites/Panama Canal/02_stem/stem.csv")
# plot_poly <- st_read("./dat/sites/Panama Canal/01_plot/plot_poly.gpkg")
# stem_wd <- read.csv("./dat/sites/Panama Canal/05_wd/stem_wd.csv")
# stem_height <- read.csv("./dat/sites/Panama Canal/06_height/stem_height.csv")

# Extract plot centres
p_cent <- st_centroid(plot_poly) %>% 
  st_transform(4326) %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(plot_id, longitude = X, latitude = Y)

# Combine dataframes
stem_all <- stem %>% 
  left_join(., stem_wd, by = "measurement_id") %>% 
  left_join(., stem_height, by = "measurement_id") %>% 
  left_join(., p_cent, by = "plot_id")

# Estimate stem biomass (Mg)
stem_all$agb <- computeAGB(
  D = stem_all$diam,
  WD = stem_all$meanWD,
  coord = stem_all[,c("longitude", "latitude")])

# Calculate basal area (m^2)
stem_all$ba <- pi * (stem_all$diam / 2)^2 / 10000 

# Create output dataframe
out <- stem_all %>% 
  dplyr::select(
    measurement_id, 
    agb,
    ba)

# Write summarised data to file
write.csv(out, file.path(outdir, "stem_agb.csv"), row.names = FALSE)

