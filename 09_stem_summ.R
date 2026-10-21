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
    # Stems without coordinates in trees with other stems that have
    # coordinates take the quadrat of the tree, see ./08_agb_mc.R
    quadrat_id = coalesce(quadrat_id_tree, quadrat_id),
    # Used to estimate AGB, and AGB density. Stems outside the quadrat grid
    # are not used to estimate AGB density
    in_agb_calc = record_id %in% record_fil,
    in_quadrat_calc = in_quadrat_calc %in% TRUE) %>%
  dplyr::select(-quadrat_id_tree) %>%
  st_as_sf(., coords = c("x_utm", "y_utm"), crs = st_crs(stem_pt),
    na.fail = FALSE, remove = FALSE)

# Write to file
st_write(stem_summ, file.path(outdir, "stem_summ.gpkg"), delete_dsn = TRUE)
