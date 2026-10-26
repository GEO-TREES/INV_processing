# Filter stem data for AGB estimation and quadrat summaries
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-21

# Select stems used to estimate AGB, see `recordFilter()` in ./func.R.
# Stems without coordinates are kept, and are assigned to quadrats at random
# during AGB error propagation in ./08_agb_mc.R. If `missing_diam_method` is
# "impute", stems without a diameter are kept, and their diameters are 
# imputed during AGB error propagation
record_fil <- recordFilter(stem, plot, 
  missing_diam = param$missing_diam_method == "impute")

# Select stems used to estimate AGB density, and to form multi-stemmed trees,
# see `agbdFilter()` in ./func.R. Stems below the minimum diameter threshold,
# the larger of `agbd_diam_min_cm` and the threshold of the plot, are
# excluded. Stems broken above the POM are excluded if `include_broken` is
# false, and stems of growth forms in `growth_form_exclude`, e.g. strangler
# figs, are excluded. Stems without a diameter are assumed to be above the
# threshold
record_agbd <- agbdFilter(stem, plot, record_fil,
  diam_min_cm = param$agbd_diam_min_cm,
  include_broken = param$include_broken,
  growth_form_exclude = param$growth_form_exclude)

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
      as.character(record_id) %in% record_agbd & is.na(diam_cm) ~ 
        "included in AGB density, diameter imputed",
      as.character(record_id) %in% record_agbd ~ "included in AGB density",
      is.na(code) | code == "" ~ "excluded: no code",
      grepl("M", code) ~ "excluded: missing",
      !grepl("A", code) ~ "excluded: dead",
      !grepl("S", code) ~ "excluded: not standing",
      grepl("T", code) ~ "excluded: broken below POM",
      is.na(diam_cm) ~ "excluded: no diameter",
      !as.character(record_id) %in% record_fil ~ "excluded: repeat measurement of stem",
      !is.na(diam_min) & !is.na(diam_cm) & diam_cm < diam_min ~ 
        "AGB only: below diameter threshold",
      grepl("B", code) & !param$include_broken ~ "AGB only: broken above POM",
      growth_form %in% param$growth_form_exclude ~
        paste0("AGB only: excluded growth form (", growth_form, ")"),
      TRUE ~ "AGB only: other")) %>%
  count(status, name = "n_record") %>%
  mutate(pct_record = round(100 * n_record / sum(n_record), 1)) %>%
  arrange(desc(grepl("^included", status)), desc(n_record))
msgTable(record_status, paste0("Stem records used to estimate AGB (",
  length(record_fil), ") and AGB density (", length(record_agbd), "), of ",
  nrow(stem), " records. \"AGB only\" stems have AGB estimates, but are not ",
  "included in AGB density:"))
write.csv(record_status, file.path(outdir, "record_status.csv"), row.names = FALSE)

# Figure of the diameter of stems used to estimate AGB density, all plots.
# Stems without a diameter are not shown
diam_hist_cm <- 5
p_diam <- ggplot(filter(stem, as.character(record_id) %in% record_agbd, !is.na(diam_cm)), 
  aes(x = diam_cm)) +
  geom_histogram(binwidth = diam_hist_cm, boundary = min(diam_min, na.rm = TRUE),
    fill = fig_pal$series[1], colour = fig_pal$surface, linewidth = 0.2) +
  labs(
    title = "Stem diameter",
    subtitle = paste0("Stems used to estimate AGBD, all plots, in ",
      diam_hist_cm, " cm classes"),
    x = "Diameter (cm)", 
    y = "Stems") +
  themeFig()
saveFig(p_diam, file.path(report_dir, "06_diam_hist.png"), width = 8, height = 5)
