# Estimate stem-level above-ground woody biomass from tree inventory data
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Multi-stemmed trees: AGB is estimated for each tree from its equivalent
# diameter, the diameter of a single stem with the same basal area as all its
# stems, then shared among its stems in proportion to their basal area. Trees
# are formed from the stems used to estimate AGB density, see `recordFilter()`
# and `treeEquiv()` in ./func.R. Other stems are treated as single-stemmed
# trees.
#
# Allometry matches that used in ./08_agb_mc.R:
#   "regional" - Chave et al. (2014) Eq. 7, using environmental stress (E) at plot centre
#   "field" - Chave et al. (2014) Eq. 4, using heights from local height-diameter model

# Stems which form multi-stemmed trees
record_fil <- recordFilter(stem, plot)

# Join wood density, height, and equivalent diameter to stem data
stem_all <- stem %>%
  left_join(., stem_wd, by = "record_id") %>%
  left_join(., stem_height, by = "record_id") %>%
  left_join(., treeEquiv(stem, record_fil), by = "record_id")

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
