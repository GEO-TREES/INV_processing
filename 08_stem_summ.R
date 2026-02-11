# Create a full stems table 
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-16

# Packages
library(dplyr)
library(sf)

# Define directories
# outdir <- "./dat/sites/Panama Canal/08_stem_out"

# Import data 
# stem <- read.csv("./dat/sites/Panama Canal/02_stem/stem.csv")
# stem_agb <- read.csv("./dat/sites/Panama Canal/07_agb/stem_agb.csv")
# stem_height <- read.csv("./dat/sites/Panama Canal/06_height/stem_height.csv")
# stem_wd <- read.csv("./dat/sites/Panama Canal/05_wd/stem_wd.csv")
# stem_taxa <- read.csv("./dat/sites/Panama Canal/03_taxa/stem_taxa.csv")
# stem_pt <- st_read("./dat/sites/Panama Canal/04_quad/stem_pt.gpkg")

# Combine stem dataframes
stem_summ <- stem %>% 
  left_join(., stem_agb, by = "record_id") %>% 
  left_join(., stem_height, by = "record_id") %>% 
  left_join(., stem_wd, by = "record_id") %>% 
  left_join(., stem_taxa, by = "record_id") %>% 
  left_join(., stem_pt, by = "record_id") %>% 
  st_sf()

# Write to file
st_write(stem_summ, file.path(outdir, "stem_summ.gpkg"), delete_dsn = TRUE) 

