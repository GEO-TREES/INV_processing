# Filter stem data for quadrat summaries
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-16

# Packages
library(dplyr)

source("./func.R")

# Define directories
# outdir <- "./dat/sites/Panama_Canal/09_stem_fil"
 
# Import data
# stem_summ <- st_read(file.path(site_data, "08_stem_summ/stem_summ.gpkg"))
# plot_poly <- st_read("./dat/sites/Panama_Canal/01_plot/plot_poly.gpkg")

# Filter stem data
stem_fil <- stem_summ %>% 
  st_transform(., 4326) %>% 
  bind_cols(., st_coordinates(.)) %>% 
  rename(
    longitude = X,
    latitude = Y) %>% 
  st_drop_geometry() %>% 
  left_join(., 
    st_drop_geometry(plot_poly)[,c("plot_id", "min_diam_thresh_cm")], 
    by = "plot_id") %>% 
  filter(
    !is.na(diam_cm),
    !is.na(quadrat_id),
    !is.na(census_id),
    diam_cm >= min_diam_thresh_cm,
    alive == TRUE,
    broken == FALSE, 
    fallen == FALSE,
    missing == FALSE) %>% 
  dplyr::select(-min_diam_thresh_cm)

# Write filtered stem data to file
write.csv(stem_fil, file.path(outdir, "stem_fil.csv"), row.names = FALSE)
