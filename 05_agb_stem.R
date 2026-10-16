# Estimate stem-level above-ground woody biomass from tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Multi-stemmed trees: AGB is estimated for each tree from its equivalent
# diameter, the diameter of a single stem with the same basal area as all its
# stems, then shared among its stems in proportion to their basal area. Trees
# are formed from the stems used to estimate AGB density, see `recordFilter()`,
# `agbdFilter()` and `treeEquiv()` in ./func.R. Other stems, e.g. stems below
# the minimum diameter threshold of the plot, are treated as single-stemmed
# trees.
#
# Allometry matches that used in ./08_agb_mc.R:
#   "regional" - Chave et al. (2014) Eq. 7, using environmental stress (E) at plot centre
#   "field" - Chave et al. (2014) Eq. 4, using heights from local height-diameter model

# Stems which form multi-stemmed trees, as in ./06_record_fil.R. If
# `multi_stem_method` is "stem", AGB is estimated for each stem separately
record_agbd <- agbdFilter(stem, plot, recordFilter(stem, plot),
  diam_min_cm = param$agbd_diam_min_cm,
  include_broken = param$include_broken)
tree_include <- if (param$multi_stem_method == "equivalent") record_agbd else character(0)

# Join wood density, height, and equivalent diameter to stem data
stem_all <- stem %>%
  left_join(., stem_wd, by = "record_id") %>%
  left_join(., stem_height, by = "record_id") %>%
  left_join(., treeEquiv(stem, tree_include), by = "record_id")

# One row per tree. Wood density depends on taxonomic identity, which is the
# same for all stems of a tree, so the wood density of the largest stem is used
tree <- stem_all %>%
  arrange(desc(ba_share)) %>%
  group_by(tree_key) %>%
  summarise(
    plot_id = first(plot_id),
    diam_eq_cm = first(diam_eq_cm),
    meanWD = first(meanWD),
    n_taxa = n_distinct(taxon_name),
    .groups = "drop")

if (any(tree$n_taxa > 1)) {
  warning(sum(tree$n_taxa > 1), " multi-stemmed trees have stems with ",
    "different taxonomic names. The wood density of the largest stem is used",
    call. = FALSE)
}

# Estimate tree-level AGB
# computeAGB() returns a list with one element per allometry
if (param$height_method == "regional") {
  tree <- left_join(tree, plot_cent, by = "plot_id")
  stopifnot(all(!is.na(tree$X)))
  tree$agb_Mg <- computeAGB(
    D = tree$diam_eq_cm,
    WD = tree$meanWD,
    coord = as.matrix(tree[, c("X", "Y")]))[[1]]
} else if (param$height_method == "field") {
  tree$agb_Mg <- computeAGB(
    D = tree$diam_eq_cm,
    WD = tree$meanWD,
    H = retrieveH(D = tree$diam_eq_cm, model = height_mod)$H)[[1]]
}

# Share tree AGB among stems
stem_agb <- stem_all %>%
  mutate(
    agb_Mg = tree$agb_Mg[match(tree_key, tree$tree_key)] * ba_share,
    ba_m2 = base::pi * (diam_cm / 2)^2 / 10000,
    volume_m3 = agb_Mg / meanWD) %>%
  dplyr::select(
    record_id,
    agb_Mg,
    ba_m2,
    volume_m3)

# Write stem-level AGB to file
write.csv(stem_agb, file.path(outdir, "stem_agb.csv"), row.names = FALSE)

# Summarise AGB of stems used to estimate AGB density in each plot, using the
# nominal plot area. Quadrat-level AGB density, from the fitted plot 
# polygons, is estimated in ./08_agb_mc.R
agb_plot <- stem %>%
  dplyr::select(record_id, plot_id) %>%
  inner_join(., stem_agb, by = "record_id") %>%
  filter(as.character(record_id) %in% record_agbd) %>%
  group_by(plot_id) %>%
  summarise(
    n_stem = n(),
    agb_Mg = sum(agb_Mg, na.rm = TRUE),
    ba_m2 = sum(ba_m2, na.rm = TRUE),
    .groups = "drop") %>%
  left_join(., dplyr::select(plot, plot_id, plot_length_m, plot_width_m), 
    by = "plot_id") %>%
  mutate(
    area_ha = plot_length_m * plot_width_m / 10000,
    agbd_Mg_ha = agb_Mg / area_ha,
    ba_m2_ha = ba_m2 / area_ha) %>%
  dplyr::select(plot_id, n_stem, area_ha, agb_Mg, agbd_Mg_ha, ba_m2_ha) %>%
  arrange(desc(agbd_Mg_ha))
msgTable(agb_plot, "AGB density in each plot, using nominal plot area:")

# Largest trees, which may indicate data errors, e.g. diameter in mm not cm
tree_large <- tree %>%
  filter(!is.na(agb_Mg)) %>%
  arrange(desc(agb_Mg)) %>%
  head(10) %>%
  dplyr::select(tree_key, plot_id, diam_eq_cm, meanWD, agb_Mg)
msgTable(tree_large, "Trees with the largest AGB:")

# Figure of tree AGB against equivalent diameter
tree_check <- filter(tree, !is.na(agb_Mg), agb_Mg > 0, diam_eq_cm > 0)
p_agb <- ggplot(tree_check, aes(x = diam_eq_cm, y = agb_Mg)) +
  geom_point(colour = fig_pal$series[1], size = 0.6, alpha = 0.3, shape = 16) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Tree AGB against diameter",
    subtitle = paste0(format(nrow(tree_check), big.mark = ","), 
      " trees. Equivalent diameter for multi-stemmed trees. Log scales; ",
      "vertical spread is due to wood density"),
    x = "Diameter (cm)",
    y = "AGB (Mg)") +
  themeFig()
saveFig(p_agb, file.path(checkdir, "05_agb_diam.png"), width = 8, height = 6)
