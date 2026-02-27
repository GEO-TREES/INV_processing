# Filter stem data for quadrat summaries
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-16

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
  group_by(site_id, plot_id, census_id, tree_id, stem_id) %>% 
  slice_max(
    order_by = tibble(pom_m, measurement_date, diam_cm), 
    n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  dplyr::select(-meas_diam_min_cm)

# Write filtered stem data to file
write.csv(stem_fil, file.path(outdir, "stem_fil.csv"), row.names = FALSE)

