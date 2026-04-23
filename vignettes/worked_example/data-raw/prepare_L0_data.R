# Prepare example data for GEO-TREES Tree Inventory workflow example
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-04-23

# Packages
library(dplyr)

# Import raw data 
stem <- read.csv("~/gdrive/geo-trees/PDA_processing/dat/sites/Amacayacu/PDA/2024-08-03/v1/v0-1/01_fmt/stem.csv")
plot <- read.csv("~/gdrive/geo-trees/PDA_processing/dat/sites/Amacayacu/PDA/2024-08-03/v1/v0-1/01_fmt/plot.csv")
plot_pt <- read.csv("~/gdrive/geo-trees/PDA_processing/dat/sites/Amacayacu/PDA/2024-08-03/v1/v0-1/01_fmt/plot_pt.csv")

# Create synthetic data

