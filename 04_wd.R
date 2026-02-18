# Estimate wood density for each measurement
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(BIOMASS)

# Source functions
source("./func.R")

# Define directories
# outdir <- "./dat/sites/Panama_Canal/04_wd"

# Import data 
# stem_taxa <- read.csv("./dat/sites/Panama_Canal/02_taxa/stem_taxa.csv")
# stem <- read.csv("./dat/sites/Panama_Canal/01_fmt/stem.csv")
wd <- read.csv("./dat/01_wd/wd.csv")

# Join taxonomy data to stem data
stem_taxa_all <- left_join(stem, stem_taxa, by = "record_id")

# Estimate wood density for each stem measurement
wd_all <- getWoodDensity(
  genus = stem_taxa_all$genusAccepted, 
  species = stem_taxa_all$speciesAccepted,
  stand = stem_taxa_all$plot_id,
  family = stem_taxa_all$familyAccepted,
  addWoodDensity = NULL,
  verbose = TRUE)

wd_out <- cbind(record_id = stem_taxa$record_id, wd_all)

# Write to file
write.csv(wd_out, file.path(outdir, "stem_wd.csv"), row.names = FALSE)
