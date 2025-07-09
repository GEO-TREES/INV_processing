# Estimate stem-level above-ground woody biomass from tree inventory data
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2025-06-18

# Define directories
outdir <- "../../dat/sites/Panama Canal/06_biomass"

# Packages
library(dplyr)
library(BIOMASS)

# Source functions
source("../../func.R")

# Import data
stems <- read.csv("../../dat/sites/Panama Canal/02_stem_fmt/stems.csv")
wd <- read.csv("../../dat/sites/Panama Canal/04_wd/wd.csv")
height <- read.csv("../../dat/sites/Panama Canal/05_height/height.csv")

# Combine dataframes
stems_all <- stems %>% 
  left_join(., wd, by = "measurement_id") %>% 
  left_join(., height, by = "measurement_id") 

# Estimate stem biomass (Mg)
stems_all$agb <- computeAGB(
  D = stems_all$diam,
  WD = stems_all$meanWD,
  H = stems_all$height_pred)

# Calculate basal area (m^2)
stems_all$ba <- pi * (stems_all$diam / 2)^2 / 10000 

# Create output dataframe
out <- stems_all %>% 
  dplyr::select(
    measurement_id, 
    agb,
    ba)

# Write summarised data to file
write.csv(out, file.path(outdir, "biomass.csv"), row.names = FALSE)

