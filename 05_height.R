# Estimate stem height
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2025-07-09

# Extract plot centres
p_cent <- plot_pt %>% 
  group_by(site_id, plot_id) %>% 
  summarise() %>% 
  st_centroid() %>% 
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
    grepl("A", code),
    grepl("S", code),
    !grepl("B", code),
    !grepl("T", code),
    !grepl("M", code)) %>% 
  dplyr::select(record_id, diam_cm, height_m)

# Write valid height measurements to file
write.csv(s_height, file.path(outdir, "stem_height_meas.csv"), row.names = FALSE)


