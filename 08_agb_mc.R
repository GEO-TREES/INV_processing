# Estimate AGB and its uncertainty in quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-14

# Errors in diameter, wood density, height and the allometric model are
# propagated to each stem with BIOMASS::AGBmonteCarlo(), then simulations are
# aggregated to quadrats with BIOMASS::subplot_summary().
#
# Multi-stemmed trees are simulated from their equivalent diameter, see
# ./05_agb_stem.R.
#
# AGBmonteCarlo() is run on groups of whole plots or quadrats, rather than on
# each quadrat separately, so that:
#   - quadrats with a single tree get uncertainty estimates
#   - the Chave et al. (2004) large diameter errors (5% of stems) are applied
#       to a meaningful number of stems
#   - stems outside the quadrat grid still get stem-level uncertainty
#
# Trees where no stems have coordinates are assigned to a random quadrat in
# their plot in each simulation, with probability proportional to quadrat
# area. This adds
# the uncertainty in their location to the quadrat AGBD estimates, while
# keeping their AGB in the plot.
#
# Allometry matches that used in ./05_agb_stem.R:
#   "regional" - Chave et al. (2014) Eq. 7, using bioclimatic variables at plot centre
#   "field" - Chave et al. (2014) Eq. 4, using local height-diameter model

# Define number of simulations
nsim <- 1000

# Set random seed, so results are reproducible
set.seed(42)

# Define maximum number of trees per AGBmonteCarlo() call, to limit memory use
# Each call holds several (n trees x nsim) matrices in memory
mc_stems_max <- 10000

# Filter stem data and add quadrat IDs
stem_fil <- stem %>%
  filter(record_id %in% record_fil) %>%
  left_join(., stem_wd, by = "record_id") %>%
  left_join(., stem_height, by = "record_id") %>%
  left_join(., st_drop_geometry(stem_pt)[, c("record_id", "quadrat_id")],
    by = "record_id")

if (param$height_method == "regional") {
  stem_fil <- left_join(stem_fil, plot_cent, by = "plot_id")
  stopifnot(all(!is.na(stem_fil$X)))
}

# Missing wood density uncertainty would cause stems to be silently dropped
# from simulated AGB sums
if (any(is.na(stem_fil$meanWD) | is.na(stem_fil$sdWD))) {
  stop("Missing `meanWD` or `sdWD` for ",
    sum(is.na(stem_fil$meanWD) | is.na(stem_fil$sdWD)), " stems")
}

# Multi-stemmed trees: AGB is simulated for each tree from its equivalent
# diameter, then shared among its stems in proportion to their basal area, as
# in ./05_agb_stem.R
stem_fil <- left_join(stem_fil, treeEquiv(stem_fil, stem_fil$record_id),
  by = "record_id")

# Stems of a tree share the quadrat of the tree, from the centroid of its
# stems, see ./07_quad.R. Stems without coordinates in trees with other stems
# that have coordinates are assigned to the quadrat of the tree. Trees where
# no stems have coordinates are assigned to quadrats at random below
stem_fil <- stem_fil %>%
  group_by(tree_key) %>%
  mutate(
    tree_mapped = any(!is.na(x_rel_m) & !is.na(y_rel_m)),
    quadrat_id = first(na.omit(quadrat_id))) %>%
  ungroup()

# One row per tree. Wood density depends on taxonomic identity, which is the
# same for all stems of a tree, so the wood density of the largest stem is used
tree <- stem_fil %>%
  arrange(desc(ba_share)) %>%
  group_by(tree_key) %>%
  summarise(
    plot_id = first(plot_id),
    quadrat_id = first(quadrat_id),
    diam_eq_cm = first(diam_eq_cm),
    meanWD = first(meanWD),
    sdWD = first(sdWD),
    across(any_of(c("X", "Y")), first),
    .groups = "drop")

# Group trees into Monte Carlo chunks
# Whole plots, split into groups of whole quadrats for very large plots
quad_chunk <- tree %>%
  count(plot_id, quadrat_id) %>%
  group_by(plot_id) %>%
  mutate(mc_chunk = (cumsum(n) - 1) %/% mc_stems_max) %>%
  ungroup() %>%
  dplyr::select(plot_id, quadrat_id, mc_chunk)

tree <- left_join(tree, quad_chunk, by = c("plot_id", "quadrat_id"))

tree_split <- split(tree, list(tree$plot_id, tree$mc_chunk), drop = TRUE)

# Run AGB Monte Carlo error propagation on each chunk
tree_simu_list <- lapply(seq_along(tree_split), function(i) {
  x <- tree_split[[i]]
  message(i, " / ", length(tree_split), " - ", x$plot_id[1],
    " (", nrow(x), " trees)")

  # AGBmonteCarlo() fails with a single tree, so duplicate the row and keep
  # the first. Each row of `AGB_simu` is simulated independently
  n_tree <- nrow(x)
  if (n_tree == 1) {
    x <- x[c(1, 1), ]
  }

  out <- AGBmonteCarlo(
    D = x$diam_eq_cm,
    WD = x$meanWD,
    errWD = x$sdWD,
    H = NULL,
    errH = NULL,
    HDmodel = if (param$height_method == "field") height_mod else NULL,
    coord = if (param$height_method == "regional") as.matrix(x[, c("X", "Y")]) else NULL,
    Dpropag = "chave2004",
    n = nsim,
    Carbon = FALSE,
    Dlim = NULL)

  out$AGB_simu[seq_len(n_tree), , drop = FALSE]
})

# Combine simulations, then share tree AGB among stems. Rows match `stem_fil`
tree <- bind_rows(tree_split)
tree_simu <- do.call(rbind, tree_simu_list)
stopifnot(nrow(tree_simu) == nrow(tree))
agb_simu <- tree_simu[match(stem_fil$tree_key, tree$tree_key), , drop = FALSE] *
  stem_fil$ba_share

# Calculate mean and standard deviation of stem AGB
agb_Mg_mean <- rowMeans(agb_simu, na.rm = TRUE)
stem_agb_mc <- data.frame(
  record_id = stem_fil$record_id,
  agb_Mg_mean = agb_Mg_mean,
  agb_Mg_sd = sqrt(rowSums((agb_simu - agb_Mg_mean)^2, na.rm = TRUE) /
    (rowSums(!is.na(agb_simu)) - 1)))

# Write stem-level data to file
write.csv(stem_agb_mc, file.path(outdir, "stem_agb_mc.csv"), row.names = FALSE)

# Format quadrat corner points
quad_pt_clean <- quad_pt %>%
  st_drop_geometry() %>%
  dplyr::select(
    plot_ID = plot_id,
    subplot_ID = quadrat_id,
    x_rel = x_rel_m,
    y_rel = y_rel_m,
    x_proj = x_utm,
    y_proj = y_utm)

# Area of each quadrat, from its corners in the order given by divide_plot(),
# as calculated by subplot_summary()
quad_area <- quad_pt_clean %>%
  group_by(plot_ID, subplot_ID) %>%
  summarise(
    area_ha = as.numeric(sf::st_area(sf::st_polygon(list(
      rbind(cbind(x_proj, y_proj), cbind(x_proj, y_proj)[1, ]))))) / 10000,
    .groups = "drop")

# Trees where no stems have coordinates
unmapped <- !stem_fil$tree_mapped

# Aggregate simulations to quadrats
# subplot_summary() is run separately for each plot, as it assigns the wrong
# plot IDs to quadrats when filling empty quadrats if plots have different
# numbers of quadrats
plot_vec <- intersect(unique(quad_pt_clean$plot_ID), unique(stem_fil$plot_id))

quad_agb_simu <- bind_rows(lapply(plot_vec, function(x) {
  quads <- quad_area[quad_area$plot_ID == x, ]

  # Stems with coordinates
  sel <- stem_fil$plot_id == x & !unmapped
  if (any(sel)) {
    subplots <- list(
      sub_corner_coord = quad_pt_clean[quad_pt_clean$plot_ID == x, ],
      tree_data = data.frame(
        plot_ID = x,
        subplot_ID = stem_fil$quadrat_id[sel]))

    ss <- subplot_summary(
      subplots = subplots,
      AGB_simu = agb_simu[sel, , drop = FALSE],
      draw_plot = FALSE)

    out <- as.data.frame(ss$long_AGB_simu) %>%
      dplyr::select(
        quadrat_id = subplot_ID,
        sim = N_simu,
        agbd_Mg_ha = AGBD)
  } else {
    out <- expand.grid(quadrat_id = quads$subplot_ID, sim = seq_len(nsim),
      stringsAsFactors = FALSE)
    out$agbd_Mg_ha <- 0
  }

  # Trees where no stems have coordinates, assigned to a random quadrat in
  # each simulation
  sel_un <- stem_fil$plot_id == x & unmapped
  if (any(sel_un)) {
    agb_un <- agb_simu[sel_un, , drop = FALSE]
    # Stems of the same tree are assigned to the same quadrat
    tree_un <- stem_fil$tree_key[sel_un]
    tree_un_id <- unique(tree_un)
    quad_idx <- matrix(
      sample(nrow(quads), length(tree_un_id) * ncol(agb_un), replace = TRUE,
        prob = quads$area_ha),
      nrow = length(tree_un_id))[match(tree_un, tree_un_id), , drop = FALSE]
    agb_quad <- vapply(seq_len(ncol(agb_un)), function(j) {
      tapply(agb_un[, j], factor(quad_idx[, j], levels = seq_len(nrow(quads))), sum, default = 0)
    }, numeric(nrow(quads)))
    agbd_quad <- agb_quad / quads$area_ha

    i <- cbind(match(out$quadrat_id, quads$subplot_ID), out$sim)
    out$agbd_Mg_ha <- out$agbd_Mg_ha + agbd_quad[i]

    message(x, ": ", length(tree_un_id), " trees without coordinates (",
      round(100 * sum(rowMeans(agb_un)) / sum(rowMeans(agb_simu[stem_fil$plot_id == x, , drop = FALSE]))),
      "% of AGB) assigned to quadrats at random")
  }

  out$plot_id <- x
  out
}))

# Write quadrat simulations to file
write.csv(quad_agb_simu, file.path(outdir, "quad_agb_mc.csv"), row.names = FALSE)

# Extract summary statistics from AGBD simulations
quad_agb <- quad_agb_simu %>%
  group_by(plot_id, quadrat_id) %>%
  summarise(
    agbd_Mg_ha_mc_mean = mean(agbd_Mg_ha),
    agbd_Mg_ha_mc_median = median(agbd_Mg_ha),
    agbd_Mg_ha_mc_sd = sd(agbd_Mg_ha),
    agbd_Mg_ha_mc_ci2.5 = quantile(agbd_Mg_ha, 0.025),
    agbd_Mg_ha_mc_ci97.5 = quantile(agbd_Mg_ha, 0.975),
    .groups = "drop")

# Write quadrat-level summary data to file
write.csv(quad_agb, file.path(outdir, "quad_agb.csv"), row.names = FALSE)
