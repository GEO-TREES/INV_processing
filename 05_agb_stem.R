# Estimate stem-level above-ground woody biomass from tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-26

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
#   "custom" - Chave et al. (2014) Eq. 4, using heights from the height-diameter
#     model in `height_custom`
# If `stem_allometry` is true, trees use the allometry in `agb_allometry` of
# their largest stem, see `agb_allometries` in ./func.R, e.g. Goodman et al.
# (2013) for palms. Trees without one use Chave et al. (2014)
#
# Diameters are corrected for taper if `taper_method` is set, see 
# ./04_height.R

# Stems which form multi-stemmed trees, as in ./06_record_fil.R. If
# `multi_stem_method` is "stem", AGB is estimated for each stem separately
record_agbd <- agbdFilter(stem, plot, 
  recordFilter(stem, plot, missing_diam = param$missing_diam_method == "impute"),
  diam_min_cm = param$agbd_diam_min_cm,
  include_broken = param$include_broken,
  growth_form_exclude = param$growth_form_exclude)
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
    agb_allometry = if (param$stem_allometry) first(agb_allometry) else NA_character_,
    n_taxa = n_distinct(taxon_name),
    .groups = "drop") %>%
  mutate(agb_allometry = coalesce(na_if(agb_allometry, ""), "chave2014"))

allom_unknown <- setdiff(tree$agb_allometry, c("chave2014", names(agb_allometries)))
if (length(allom_unknown) > 0) {
  stop("Unknown `agb_allometry` in the stem table: ", 
    paste(allom_unknown, collapse = ", "), ". Must be one of: ",
    paste(c("chave2014", names(agb_allometries)), collapse = ", "))
}

if (any(tree$n_taxa > 1)) {
  warning(sum(tree$n_taxa > 1), " multi-stemmed trees have stems with ",
    "different taxonomic names. The wood density of the largest stem is used",
    call. = FALSE)
}

# Estimate tree-level AGB
# computeAGB() returns a vector of tree AGB in Mg. Stems without a diameter,
# see `missing_diam_method` in param.yaml, have no AGB estimate here. Their 
# diameters are imputed in ./08_agb_mc.R
muffleNA <- function(w) {
  if (grepl("NA values in D", conditionMessage(w))) invokeRestart("muffleWarning")
}
if (param$height_method == "regional") {
  tree <- left_join(tree, plot_cent, by = "plot_id")
  stopifnot(all(!is.na(tree$X)))
  tree$agb_Mg <- withCallingHandlers(computeAGB(
    D = tree$diam_eq_cm,
    WD = tree$meanWD,
    coord = as.matrix(tree[, c("X", "Y")])), warning = muffleNA)
} else if (param$height_method %in% c("field", "custom")) {
  tree$agb_Mg <- withCallingHandlers(computeAGB(
    D = tree$diam_eq_cm,
    WD = tree$meanWD,
    H = retrieveH(D = tree$diam_eq_cm, model = height_mod)$H), warning = muffleNA)
}

# Trees with other allometries, see `stem_allometry`
for (a in intersect(names(agb_allometries), tree$agb_allometry)) {
  i <- which(tree$agb_allometry == a)
  tree$agb_Mg[i] <- agbAllomMean(agb_allometries[[a]],
    D = tree$diam_eq_cm[i],
    H = if (exists("height_mod")) retrieveH(D = tree$diam_eq_cm[i], model = height_mod)$H,
    WD = tree$meanWD[i])
}
if (param$stem_allometry) {
  msgTable(count(tree, agb_allometry, name = "n_tree"), "Trees by AGB allometry:")
}

# Share tree AGB among stems
stem_agb <- stem_all %>%
  mutate(
    agb_Mg = tree$agb_Mg[match(tree_key, tree$tree_key)] * ba_share,
    # Basal area from the measured diameter
    ba_m2 = base::pi * (diam_cm_meas / 2)^2 / 10000,
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
  # Plots with no stems used to estimate AGB density have zero AGB
  right_join(., dplyr::select(plot, plot_id, plot_length_m, plot_width_m), 
    by = "plot_id") %>%
  mutate(
    across(c(n_stem, agb_Mg, ba_m2), ~ifelse(is.na(.x), 0, .x)),
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
      " trees. Equivalent diameter for multi-stemmed trees. Log scales"),
    x = "Diameter (cm)",
    y = "AGB (Mg)") +
  themeFig()
saveFig(p_agb, file.path(report_dir, "05_agb_diam.png"), width = 8, height = 6)

# Figure of the share of AGB in each diameter class, for trees used to
# estimate AGB density, all plots. Classes from `diam_class_max_cm` upwards
# are combined
diam_class_cm <- 10
diam_class_max_cm <- 150
tree_agbd <- tree %>%
  filter(tree_key %in% stem_all$tree_key[as.character(stem_all$record_id) %in% record_agbd],
    !is.na(agb_Mg))
diam_class <- tree_agbd %>%
  mutate(diam_class = pmin(floor(diam_eq_cm / diam_class_cm) * diam_class_cm, 
    diam_class_max_cm)) %>%
  group_by(diam_class) %>%
  summarise(n_tree = n(), agb_Mg = sum(agb_Mg), .groups = "drop") %>%
  mutate(
    pct_agb = 100 * agb_Mg / sum(agb_Mg),
    label = ifelse(diam_class == diam_class_max_cm, paste0(diam_class_max_cm, "+"),
      paste0(diam_class, "-", diam_class + diam_class_cm)),
    label = factor(label, levels = label))
p_diam_class <- ggplot(diam_class, aes(x = label, y = pct_agb)) +
  geom_col(fill = fig_pal$series[1], width = 0.8) +
  geom_text(aes(label = format(n_tree, big.mark = ",", trim = TRUE)),
    vjust = -0.4, size = 3, colour = fig_pal$ink_muted) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Share of AGB by diameter class",
    subtitle = paste0("Trees used to estimate AGBD, all plots. Numbers are ",
      "trees in each class. Equivalent diameter for multi-stemmed trees"),
    x = "Diameter class (cm)",
    y = "Share of AGB (%)") +
  themeFig() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
saveFig(p_diam_class, file.path(report_dir, "05_agb_diam_class.png"), width = 8, height = 5)
