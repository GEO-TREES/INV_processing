# Estimate stem height
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(sf)
library(BIOMASS)

# Define directories
outdir <- "../../dat/sites/Bicuar/05_height"

# Import data
s <- read.csv("../../dat/sites/Bicuar/02_stem_fmt/stems.csv")
polys <- st_read("../../dat/sites/Bicuar/01_polys/polys.gpkg")

# Extract plot centres
p_cent <- st_centroid(polys) %>% 
  st_transform(4326) %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(plot_id, X, Y)

# Add plot centres to stem data
s_cent <- s %>% 
  left_join(., p_cent, by = "plot_id")

stopifnot(all(!is.na(s_cent$X)))
stopifnot(all(!is.na(s_cent$Y)))

# Retrieve stem heights using plot locations
s_cent$height_pred <- retrieveH(s_cent$diam, coord = s_cent[,c("X", "Y")])$H

# Create output dataframe
out <- s_cent %>% 
  dplyr::select(measurement_id, height_pred)

# Write to file
write.csv(out, file.path(outdir, "height.csv"), row.names = FALSE)

