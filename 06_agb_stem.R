# Estimate stem-level above-ground woody biomass from tree inventory data
# John L. Godlee (johngodlee@gmail.com)  
# Last updated: 2026-02-16

# Define directories
# outdir <- "./dat/sites/PanamaCanal/06_agb_stem"

# Packages
library(dplyr)
library(BIOMASS)

# Source functions
source("./func.R")

# Import data
# stem <- read.csv("./dat/sites/PanamaCanal/01_fmt/stem.csv")
# stem_wd <- read.csv("./dat/sites/PanamaCanal/04_wd/stem_wd.csv")
# stem_height <- read.csv("./dat/sites/PanamaCanal/05_height/stem_height.csv")

# Estimate stem-level AGB
stem_agb <- stem %>% 
  left_join(., stem_wd, by = "record_id") %>% 
  left_join(., stem_height, by = "record_id") %>% 
  mutate(
    agb_Mg = computeAGB(
      D = .$diam_cm,
      WD = .$meanWD,
      H = .$height_m_pred),
    ba_m2 = pi * (.$diam_cm / 2)^2 / 10000) %>% 
  dplyr::select(
    record_id, 
    agb_Mg,
    ba_m2)

# Write stem-level AGB to file
write.csv(stem_agb, file.path(outdir, "stem_agb.csv"), row.names = FALSE)

