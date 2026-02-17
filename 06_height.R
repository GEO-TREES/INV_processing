# Estimate stem height
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Packages
library(dplyr)
library(sf)
library(BIOMASS)

# Define directories
# outdir <- "./dat/sites/Panama_Canal/06_height"

# Import data
# stem <- read.csv("./dat/sites/Panama_Canal/02_stem/stem.csv")
# plot_poly <- st_read("./dat/sites/Panama_Canal/01_plot/plot_poly.gpkg")

# Extract plot centres
p_cent <- st_centroid(plot_poly) %>% 
  st_transform(4326) %>% 
  cbind(., st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(plot_id, X, Y)

# Add plot centres to stem data
s_cent <- stem %>% 
  left_join(., p_cent, by = "plot_id")

stopifnot(all(!is.na(s_cent$X)))
stopifnot(all(!is.na(s_cent$Y)))

# Retrieve stem heights using plot locations
s_cent$height_m_pred <- retrieveH(
  D = s_cent$diam_cm, 
  coord = s_cent[,c("X", "Y")])$H

# Create output dataframe
out <- s_cent %>% 
  dplyr::select(record_id, height_m_pred)

# Write to file
write.csv(out, file.path(outdir, "stem_height.csv"), row.names = FALSE)

# Extract valid height field measurements
s_height <- s_cent %>% 
  filter(
    !is.na(diam_cm),
    !is.na(height_m),
    alive == TRUE,
    broken == FALSE,
    fallen == FALSE,
    missing == FALSE) %>% 
  dplyr::select(record_id, diam_cm, height_m)

# Write valid height measurements to file
write.csv(s_height, file.path(outdir, "stem_height_meas.csv"), row.names = FALSE)


