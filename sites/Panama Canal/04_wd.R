# Estimate wood density for each measurement
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(BIOMASS)

# Source functions
source("../../func.R")

# Define directories
outdir <- "../../dat/sites/Panama Canal/04_wd"

# Import data 
taxa <- read.csv("../../dat/sites/Panama Canal/03_taxa/taxa.csv")
stems <- read.csv("../../dat/sites/Panama Canal/02_stem_fmt/stems.csv")
wd <- read.csv("../../dat/01_wd/wd.csv")

# Join taxonomy data to stem data
stems_taxa <- left_join(stems, taxa, by = "measurement_id")

# Estimate wood density for each stem measurement
wd_out <- wdGen(stems_taxa, wd, 
  regional = FALSE,
  measurement_id = "measurement_id",
  site_id = "site_id",
  plot_id = "plot_id",
  family = "taxon_family_acc",
  genus = "taxon_genus_acc",
  species = "taxon_species_acc",
  wd = "wd", 
  wd_region = "wd_region")

# Write to file
write.csv(wd_out, file.path(outdir, "wd.csv"), row.names = FALSE)

