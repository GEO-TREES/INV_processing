# Filter stem data for quadrat summaries
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-16

# Packages
library(dplyr)

source("./func.R")

# Define directories
# outdir <- "./dat/sites/PanamaCanal/08_stem_fil"
 
# Import data
# stem_summ <- st_read(file.path(site_data, "07_stem_summ/stem_summ.gpkg"))

# Filter stem data
stem_fil <- stem_summ %>% 
  st_transform(., 4326) %>% 
  bind_cols(., st_coordinates(.)) %>% 
  rename(
    longitude = X,
    latitude = Y) %>% 
  st_drop_geometry() %>% 
  filter(
    !is.na(diam_cm),
    !is.na(quadrat_id),
    !is.na(census_id),
    diam_cm >= meas_diam_min_cm,
    alive == TRUE,
    broken == FALSE, 
    fallen == FALSE,
    missing == FALSE) %>% 
  dplyr::select(-meas_diam_min_cm)

# Write filtered stem data to file
write.csv(stem_fil, file.path(outdir, "stem_fil.csv"), row.names = FALSE)
