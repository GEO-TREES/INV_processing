# Create a full stems table 
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-16

# Packages
library(dplyr)
library(sf)

# Define directories
# outdir <- "./dat/sites/Panama Canal/08_stem_out"

# Import data 
# stems <- read.csv("./dat/sites/Panama Canal/02_stem_fmt/stems.csv")
# biomass <- read.csv("./dat/sites/Panama Canal/07_biomass/biomass.csv")
# height <- read.csv("./dat/sites/Panama Canal/06_height/height.csv")
# wd <- read.csv("./dat/sites/Panama Canal/05_wd/wd.csv")
# taxa <- read.csv("./dat/sites/Panama Canal/03_taxa/taxa.csv")
# stems_coords <- st_read("./dat/sites/Panama Canal/04_subplots/stems_coords.gpkg")

# Combine stem dataframes
stems_all <- stems %>% 
  left_join(., biomass, by = "measurement_id") %>% 
  left_join(., height, by = "measurement_id") %>% 
  left_join(., wd, by = "measurement_id") %>% 
  left_join(., taxa, by = "measurement_id") %>% 
  left_join(., stems_coords, by = "measurement_id")

# Write to file
st_write(stems_all, file.path(outdir, "stems_all.gpkg"), delete_dsn = TRUE) 

