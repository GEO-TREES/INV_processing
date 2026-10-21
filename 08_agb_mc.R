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
# Stems below the minimum diameter threshold of the plot are included in
# error propagation, but not in quadrat AGB density.
#
# Trees where no stems have coordinates are assigned to a random quadrat in
# their plot in each simulation, with probability proportional to quadrat
# area. This adds
# the uncertainty in their location to the quadrat AGBD estimates, while
# keeping their AGB in the plot.
#
# Diameter measurement error is set by `diam_error` in param.yaml. With
# "field", a diameter measurement error model is estimated from repeat
# diameter measurements in `stem_dup.csv` with BIOMASS::fitDiamError().
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
tree_include <- if (param$multi_stem_method == "equivalent") record_agbd else character(0)
stem_fil <- left_join(stem_fil, treeEquiv(stem_fil, tree_include),
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

# Diameter measurement error, see `diam_error` in param.yaml
if (identical(param$diam_error, "none")) {
  diam_error <- NULL
} else if (identical(param$diam_error, "field")) {
  # Minimum number of repeat measurements needed to estimate the model, and to
  # estimate the proportion of large errors
  dup_n_min <- 30
  dup_n_large <- 200

  # Maximum difference between original and repeat measurements in cm.
  # Larger differences, more than 4 standard deviations of the large errors of
  # Chave et al. (2004), are treated as data errors, e.g. a misplaced decimal
  # point, which should be corrected in the raw data rather than propagated as
  # measurement error
  dup_max_diff_cm <- 20

  # Pair repeat measurements with their original measurements, matched by
  # `plot_id` and `stem_id`, see `dupMatch()` in ./func.R. Pairs at a
  # different POM, more than one year apart, or with very large differences
  # are not used
  dup_i <- dupMatch(stem_dup, stem)
  diam_dup <- stem_dup %>%
    dplyr::select(record_id_dup = record_id, plot_id, stem_id,
      diam_cm_dup = diam_cm, pom_m_dup = pom_m,
      measurement_date_dup = measurement_date, any_of("team")) %>%
    mutate(
      record_id = stem$record_id[dup_i],
      tree_id = stem$tree_id[dup_i],
      diam_cm = stem$diam_cm[dup_i],
      pom_m = stem$pom_m[dup_i],
      measurement_date = stem$measurement_date[dup_i],
      .after = stem_id) %>%
    mutate(status = case_when(
      is.na(record_id) ~ "excluded: stem not found",
      is.na(diam_cm) ~ "excluded: no original diameter",
      !is.na(pom_m) & !is.na(pom_m_dup) & abs(pom_m - pom_m_dup) > 0.05 ~
        "excluded: different POM",
      !is.na(measurement_date) & !is.na(measurement_date_dup) &
        abs(measurement_date - measurement_date_dup) > 1 ~
        "excluded: more than one year apart",
      abs(diam_cm - diam_cm_dup) > dup_max_diff_cm ~
        "excluded: likely data error",
      TRUE ~ "used"))
  if (any(diam_dup$status == "excluded: likely data error")) {
    warning(sum(diam_dup$status == "excluded: likely data error"),
      " repeat diameter measurements differ from the original measurement by ",
      "more than ", dup_max_diff_cm, " cm, and are not used. Check ",
      file.path(checkdir, "diam_dup.csv"), call. = FALSE)
  }
  dup_used <- diam_dup$status == "used"

  if (sum(dup_used) < dup_n_min) {
    warning("Only ", sum(dup_used), " repeat diameter measurements, at least ",
      dup_n_min, " are needed. Using \"chave2004\" diameter measurement error",
      call. = FALSE)
    diam_error <- "chave2004"
    diam_dup$prob_large <- NA_real_
  } else {
    # Original and repeat measurements are assumed to have the same error, so
    # the model describes the error of a single measurement
    diam_error <- fitDiamError(
      D1 = diam_dup$diam_cm[dup_used],
      D2 = diam_dup$diam_cm_dup[dup_used],
      large = sum(dup_used) >= dup_n_large)
    print(diam_error)

    diam_dup$prob_large <- NA_real_
    diam_dup$prob_large[dup_used] <- diam_error$pairs$prob_large

    # With few repeat measurements, large errors can't be estimated, so the
    # proportion and standard deviation of large errors from Chave et al.
    # (2004) are used
    if (!diam_error$large) {
      message("Fewer than ", dup_n_large, " repeat diameter measurements, ",
        "large diameter errors from Chave et al. (2004) are used")
      diam_error$p_large <- 0.05
      diam_error$sd_large <- 4.64
    }

    # A systematic difference between measurements is not propagated
    if (!is.na(diam_error$bias_se) &&
      abs(diam_error$bias) > 2 * diam_error$bias_se) {
      warning("Systematic difference between original and repeat diameter ",
        "measurements of ", signif(diam_error$bias, 2), " cm, this is not ",
        "propagated to AGB. Check ", file.path(checkdir, "diam_dup.csv"),
        call. = FALSE)
    }

    # The model is applied to the equivalent diameter of multi-stemmed trees.
    # Warn if trees are much larger than the repeat measurements
    n_out <- sum(tree$diam_eq_cm > diam_error$D_range[2], na.rm = TRUE)
    if (n_out > 0) {
      warning(n_out, " trees are larger than the largest repeat diameter ",
        "measurement (", round(diam_error$D_range[2], 1), " cm), diameter ",
        "measurement error is extrapolated", call. = FALSE)
    }

    # Write diameter measurement error model parameters to file
    write.csv(data.frame(
      a = diam_error$a,
      b = diam_error$b,
      p_large = diam_error$p_large,
      sd_large = diam_error$sd_large,
      large_estimated = diam_error$large,
      bias_cm = diam_error$bias,
      bias_se_cm = diam_error$bias_se,
      n_pairs = diam_error$n,
      diam_min_cm = diam_error$D_range[1],
      diam_max_cm = diam_error$D_range[2]),
      file.path(outdir, "diam_error.csv"), row.names = FALSE)
  }

  # Write repeat measurements to file, to check raw data. Excluded pairs are
  # listed first, then pairs with a high `prob_large`, which may include a
  # data error
  diam_dup %>%
    mutate(diff_cm = diam_cm - diam_cm_dup) %>%
    arrange(status == "used", desc(prob_large), desc(abs(diff_cm))) %>%
    write.csv(., file.path(checkdir, "diam_dup.csv"), row.names = FALSE)
} else {
  diam_error <- param$diam_error
}

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
    Dpropag = diam_error,
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

# Stems below the minimum diameter threshold of the plot are used to estimate
# AGB, but are not included in AGB density
in_agbd <- stem_fil$record_id %in% record_agbd

# Write trees without coordinates to file, to check raw data
tree_unmapped <- stem_fil[unmapped, ] %>%
  mutate(agb_Mg_mean = agb_Mg_mean[unmapped]) %>%
  group_by(plot_id, tree_id, tree_key) %>%
  summarise(
    stem_id = paste(stem_id, collapse = ", "),
    n_stem = n(),
    diam_eq_cm = first(diam_eq_cm),
    agb_Mg_mean = sum(agb_Mg_mean),
    .groups = "drop") %>%
  dplyr::select(-tree_key)
write.csv(tree_unmapped, file.path(checkdir, "tree_unmapped.csv"), row.names = FALSE)

# Aggregate simulations to quadrats
# subplot_summary() is run separately for each plot, as it assigns the wrong
# plot IDs to quadrats when filling empty quadrats if plots have different
# numbers of quadrats
plot_vec <- intersect(unique(quad_pt_clean$plot_ID), unique(stem_fil$plot_id))

quad_agb_simu <- bind_rows(lapply(plot_vec, function(x) {
  quads <- quad_area[quad_area$plot_ID == x, ]

  # Stems with coordinates
  sel <- stem_fil$plot_id == x & !unmapped & in_agbd
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
  # If `unmapped_method` is "exclude", they are not included in AGB density
  sel_un <- stem_fil$plot_id == x & unmapped & in_agbd
  if (param$unmapped_method == "random" && any(sel_un)) {
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

# Summarise plot-level AGB density and its uncertainty, from the area-weighted
# mean of quadrat AGB density in each simulation. This is compared to the sum
# of stem AGB from ./05_agb_stem.R over the same area. The two should be 
# similar. Differences arise from stems excluded from quadrats, and because
# measurement errors are not symmetric on the AGB scale
quad_area_join <- quad_area %>%
  transmute(plot_id = as.character(plot_ID), quadrat_id = subplot_ID, area_ha)

plot_agb_mc <- quad_agb_simu %>%
  mutate(plot_id = as.character(plot_id)) %>%
  inner_join(., quad_area_join, by = c("plot_id", "quadrat_id")) %>%
  group_by(plot_id, sim) %>%
  summarise(agbd_Mg_ha = sum(agbd_Mg_ha * area_ha) / sum(area_ha), 
    .groups = "drop") %>%
  group_by(plot_id) %>%
  summarise(
    agbd_Mg_ha_mc_mean = mean(agbd_Mg_ha),
    agbd_Mg_ha_mc_sd = sd(agbd_Mg_ha),
    agbd_Mg_ha_mc_ci2.5 = quantile(agbd_Mg_ha, 0.025),
    agbd_Mg_ha_mc_ci97.5 = quantile(agbd_Mg_ha, 0.975),
    .groups = "drop")

plot_agb_det <- stem %>%
  dplyr::select(record_id, plot_id) %>%
  inner_join(., stem_agb, by = "record_id") %>%
  filter(as.character(record_id) %in% record_agbd) %>%
  mutate(plot_id = as.character(plot_id)) %>%
  group_by(plot_id) %>%
  summarise(agb_Mg = sum(agb_Mg, na.rm = TRUE), .groups = "drop") %>%
  inner_join(., quad_area_join %>% group_by(plot_id) %>% 
    summarise(area_ha = sum(area_ha), n_quadrat = n(), .groups = "drop"),
    by = "plot_id") %>%
  mutate(agbd_Mg_ha_stem_sum = agb_Mg / area_ha)

plot_agb_check <- plot_agb_det %>%
  dplyr::select(plot_id, n_quadrat, area_ha, agbd_Mg_ha_stem_sum) %>%
  inner_join(., plot_agb_mc, by = "plot_id") %>%
  mutate(
    cv_pct = round(100 * agbd_Mg_ha_mc_sd / agbd_Mg_ha_mc_mean, 1),
    diff_pct = round(100 * (agbd_Mg_ha_mc_mean - agbd_Mg_ha_stem_sum) / 
      agbd_Mg_ha_stem_sum, 1)) %>%
  arrange(desc(agbd_Mg_ha_mc_mean))
msgTable(plot_agb_check, paste0("Plot AGB density (Mg/ha) over the quadrat ",
  "grid, from ", nsim, " simulations (mc), and the sum of stem AGB ",
  "(stem_sum). diff_pct is the difference between them:"))

if (any(abs(plot_agb_check$diff_pct) > 10, na.rm = TRUE)) {
  warning("Simulated plot AGB density differs from the sum of stem AGB by ",
    "more than 10% in plots: ", 
    paste(plot_agb_check$plot_id[abs(plot_agb_check$diff_pct) > 10], collapse = ", "),
    ". Check stems outside the quadrat grid, see ", 
    file.path(checkdir, "stem_outside.csv"), call. = FALSE)
}

# Figure of plot AGB density with 95% interval from simulations, and the 
# sum of stem AGB
plot_agb_fig <- plot_agb_check %>%
  mutate(plot_id = factor(plot_id, levels = rev(levels(plotFactor(plot_id)))))
p_plot_agb <- ggplot(plot_agb_fig, aes(y = plot_id)) +
  geom_linerange(aes(xmin = agbd_Mg_ha_mc_ci2.5, xmax = agbd_Mg_ha_mc_ci97.5),
    colour = fig_pal$series[1], linewidth = 0.8) +
  geom_point(aes(x = agbd_Mg_ha_mc_mean, shape = "Simulated mean, 95% interval"),
    colour = fig_pal$series[1], size = 2.4) +
  geom_point(aes(x = agbd_Mg_ha_stem_sum, shape = "Sum of stem AGB"),
    colour = fig_pal$ink, size = 2.4, stroke = 0.7) +
  scale_shape_manual(NULL, values = c("Simulated mean, 95% interval" = 16, 
    "Sum of stem AGB" = 1)) +
  labs(
    title = "Plot AGB density",
    subtitle = paste0("Over the quadrat grid of each plot. ", nsim, 
      " simulations, diameter error: ", 
      if (is.list(diam_error)) "field" else format(param$diam_error)),
    x = "AGB density (Mg/ha)",
    y = "Plot") +
  themeFig()
saveFig(p_plot_agb, file.path(checkdir, "08_plot_agbd.png"), width = 8,
  height = min(max(2.6 + 0.35 * nrow(plot_agb_fig), 4), 24))
