# Filter stem data for quadrat summaries
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-02-16

# Filter stem data
record_fil <- stem %>% 
  left_join(., stem_pt, by = "record_id") %>% 
  left_join(., plot[,c("plot_id", "meas_diam_min_cm")], 
    by = "plot_id") %>% 
  filter(
    !is.na(diam_cm),
    !is.na(quadrat_id),
    diam_cm >= meas_diam_min_cm,
    grepl("A", code),
    grepl("S", code),
    !grepl("B", code),
    !grepl("T", code),
    !grepl("M", code)
  ) %>% 
  group_by(site_id, plot_id, tree_id, stem_id) %>% 
  slice_max(
    order_by = tibble(pom_m, measurement_date, diam_cm), 
    n = 1, 
    with_ties = FALSE) %>%
  ungroup() %>% 
  pull(record_id) %>% 
  as.character()

# Write filtered stem data to file
writeLines(record_fil, file.path(outdir, "record_fil.txt"))

