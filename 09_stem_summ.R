# Create a full stems table
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Combine stem dataframes
# Stems without coordinates have empty point geometries
stem_summ <- stem %>%
  left_join(., taxa, by = c("taxon_name" = "nameOriginal")) %>%
  left_join(., stem_wd, by = "record_id") %>%
  left_join(., stem_height, by = "record_id") %>%
  left_join(., stem_agb, by = "record_id") %>%
  left_join(., stem_agb_mc, by = "record_id") %>%
  left_join(., st_drop_geometry(stem_pt), by = "record_id") %>%
  mutate(
    in_quadrat_calc = ifelse(record_id %in% record_fil, TRUE, FALSE)) %>%
  st_as_sf(., coords = c("x_utm", "y_utm"), crs = st_crs(stem_pt),
    na.fail = FALSE, remove = FALSE)

# Write to file
st_write(stem_summ, file.path(outdir, "stem_summ.gpkg"), delete_dsn = TRUE)
