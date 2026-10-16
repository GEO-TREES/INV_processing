# Filter stem data for AGB estimation and quadrat summaries
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Select stems used to estimate AGB, see `recordFilter()` in ./func.R.
# Stems without coordinates are kept, and are assigned to quadrats at random
# during AGB error propagation in ./08_agb_mc.R
record_fil <- recordFilter(stem, plot)

# Select stems used to estimate AGB density, and to form multi-stemmed trees,
# see `agbdFilter()` in ./func.R. Stems below the minimum diameter threshold,
# the larger of `agbd_diam_min_cm` and the threshold of the plot, are
# excluded. Stems broken above the POM are excluded if `include_broken` is
# false
record_agbd <- agbdFilter(stem, plot, record_fil,
  diam_min_cm = param$agbd_diam_min_cm,
  include_broken = param$include_broken)

# Write filtered stem data to file
writeLines(record_fil, file.path(outdir, "record_fil.txt"))
writeLines(record_agbd, file.path(outdir, "record_agbd.txt"))

# Summarise why stem records are excluded from AGB estimates and AGB density.
# Each record is counted once, under the first reason which applies, in the 
# order of the checks in `recordFilter()` and `agbdFilter()`
diam_min <- pmax(plot$meas_diam_min_cm[match(stem$plot_id, plot$plot_id)],
  param$agbd_diam_min_cm)
record_status <- stem %>%
  mutate(
    status = case_when(
      as.character(record_id) %in% record_agbd ~ "included in AGB density",
      is.na(code) | code == "" ~ "excluded: no code",
      grepl("M", code) ~ "excluded: missing",
      !grepl("A", code) ~ "excluded: dead",
      !grepl("S", code) ~ "excluded: not standing",
      grepl("T", code) ~ "excluded: broken below POM",
      is.na(diam_cm) ~ "excluded: no diameter",
      !as.character(record_id) %in% record_fil ~ "excluded: repeat measurement of stem",
      !is.na(diam_min) & diam_cm < diam_min ~ "AGB only: below diameter threshold",
      grepl("B", code) & !param$include_broken ~ "AGB only: broken above POM",
      TRUE ~ "AGB only: other")) %>%
  count(status, name = "n_record") %>%
  mutate(pct_record = round(100 * n_record / sum(n_record), 1)) %>%
  arrange(desc(status == "included in AGB density"), desc(n_record))
msgTable(record_status, paste0("Stem records used to estimate AGB (",
  length(record_fil), ") and AGB density (", length(record_agbd), "), of ",
  nrow(stem), " records. \"AGB only\" stems have AGB estimates, but are not ",
  "included in AGB density:"))

# Figure of stem diameter distribution in each plot, stems per hectare in 
# 10 cm diameter classes, for stems used to estimate AGB at or above the 
# minimum diameter threshold of each plot. Classes start at the threshold. 
# Uses the nominal plot area
diam_dist <- stem %>%
  mutate(diam_min_cm = ifelse(is.na(diam_min), 0, diam_min)) %>%
  filter(
    as.character(record_id) %in% record_fil,
    diam_cm >= diam_min_cm) %>%
  mutate(diam_class_cm = floor((diam_cm - diam_min_cm) / 10) * 10 + diam_min_cm + 5) %>%
  count(plot_id, diam_class_cm, name = "n_stem") %>%
  left_join(., dplyr::select(plot, plot_id, plot_length_m, plot_width_m), 
    by = "plot_id") %>%
  mutate(stem_ha = n_stem / (plot_length_m * plot_width_m / 10000))
n_plot_diam <- length(unique(diam_dist$plot_id))
diam_min_fig <- min(diam_min[stem$plot_id %in% diam_dist$plot_id], na.rm = TRUE)
p_diam <- ggplot(diam_dist, aes(x = diam_class_cm, y = stem_ha)) +
  geom_point(colour = fig_pal$series[1], size = 2) +
  scale_x_continuous(limits = c(diam_min_fig, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_y_log10() +
  facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_diam)) +
  labs(
    title = "Stem diameter distribution",
    subtitle = paste0("Stems used to estimate AGB, at or above the minimum ",
      "diameter threshold, in 10 cm classes. Log scale. Nominal plot area"),
    x = "Diameter class midpoint (cm)",
    y = "Stems per hectare") +
  themeFig()
saveFig(p_diam, file.path(checkdir, "06_diam_dist.png"), n_panel = n_plot_diam)
