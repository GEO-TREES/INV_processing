# Estimate AGB and its uncertainty in quadrats
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-10-26

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
# Trees where no stems have coordinates are placed at a random location in
# each simulation, within their field subplot if its position is known, 
# otherwise within their plot, with BIOMASS::subplot_summary(tree_bounds = ).
# This adds the uncertainty in their location to the quadrat AGBD estimates.
# See `unmapped_method` in param.yaml.
#
# Stems without a diameter measurement have their diameter imputed in each
# simulation by BIOMASS::AGBmonteCarlo(), from the simulated diameters of the
# other trees in the same Monte Carlo chunk with the same AGB allometry, i.e.
# the same plot, or the same group of quadrats for very large plots. This
# adds the uncertainty in their size to the quadrat AGBD estimates. See
# `missing_diam_method` in param.yaml.
#
# Diameter measurement error is set by `diam_error` in param.yaml. With
# "field", a diameter measurement error model is estimated from repeat
# diameter measurements in `stem_dup.csv` with BIOMASS::fitDiamError().
#
# Allometry matches that used in ./05_agb_stem.R:
#   "regional" - Chave et al. (2014) Eq. 7, using bioclimatic variables at plot centre
#   "field" - Chave et al. (2014) Eq. 4, using local height-diameter model
#   "custom" - Chave et al. (2014) Eq. 4, using the height-diameter model in
#     `height_custom`
# If `stem_allometry` is true, trees use the allometry in `agb_allometry` of
# their largest stem, see `agb_allometries` in ./func.R. Each allometry is
# simulated separately, with its own residual error
#
# Diameters are corrected for taper if `taper_method` is set, see
# ./04_height.R
#
# If `crown_method` is "custom", quadrat AGB density is also estimated with
# the AGB of each tree spread over its crown, from the crown area allometry
# in `crown_custom`, in the same simulations, see below

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
  left_join(., st_drop_geometry(stem_pt)[, c("record_id", "quadrat_id", "grid_excl")],
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
    # Stems with coordinates, including stems given the mean coordinates of
    # the other stems of their tree in ./07_quad.R
    tree_mapped = any(record_id %in% stem_pt$record_id),
    quadrat_id = first(na.omit(quadrat_id)),
    # In a strip of the plot left out of the quadrat grid, see ./07_quad.R
    grid_excl = any(grid_excl %in% TRUE)) %>%
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
    agb_allometry = if (param$stem_allometry) first(agb_allometry) else NA_character_,
    across(any_of(c("X", "Y")), first),
    .groups = "drop") %>%
  mutate(agb_allometry = coalesce(na_if(agb_allometry, ""), "chave2014"))

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
      file.path(report_dir, "diam_dup.csv"), call. = FALSE)
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
        "propagated to AGB. Check ", file.path(report_dir, "diam_dup.csv"),
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
    write.csv(., file.path(report_dir, "diam_dup.csv"), row.names = FALSE)

  # Figure of differences between repeat diameter measurements, with the
  # fitted measurement error model. The difference between two measurements,
  # each with standard deviation s, has standard deviation sqrt(2) * s
  if (is.list(diam_error)) {
    dup_fig <- diam_dup %>%
      filter(status == "used") %>%
      mutate(
        diam_mean_cm = (diam_cm + diam_cm_dup) / 2,
        diff_cm = diam_cm - diam_cm_dup,
        type = ifelse(!is.na(prob_large) & prob_large > 0.5,
          "Likely large error", "Small error"))
    dup_curve <- data.frame(diam_mean_cm = seq(min(dup_fig$diam_mean_cm),
      max(dup_fig$diam_mean_cm), length.out = 200)) %>%
      mutate(
        upper = 1.96 * sqrt(2) * (diam_error$a + diam_error$b * diam_mean_cm),
        lower = -upper)
    p_dup <- ggplot() +
      geom_hline(yintercept = 0, colour = fig_pal$line_muted) +
      geom_point(data = dup_fig, aes(x = diam_mean_cm, y = diff_cm, colour = type),
        size = 1.2, alpha = 0.6, shape = 16) +
      geom_line(data = dup_curve, aes(x = diam_mean_cm, y = upper),
        colour = fig_pal$ink, linewidth = 0.6, linetype = 2) +
      geom_line(data = dup_curve, aes(x = diam_mean_cm, y = lower),
        colour = fig_pal$ink, linewidth = 0.6, linetype = 2) +
      scale_colour_manual(NULL, values = c("Small error" = fig_pal$series[1],
        "Likely large error" = fig_pal$warn)) +
      guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
      labs(
        title = "Repeat diameter measurements",
        subtitle = paste0(nrow(dup_fig), " pairs. Dashed lines: 95% range of ",
          "differences from small errors, SD = ", signif(diam_error$a, 3),
          " + ", signif(diam_error$b, 3), " D cm per measurement"),
        x = "Mean diameter (cm)",
        y = "Difference between measurements (cm)") +
      themeFig()
    saveFig(p_dup, file.path(report_dir, "08_diam_error.png"), width = 8, height = 6)
  }
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

# Simulate AGB of trees `x` which share the AGB allometry `allom`, see
# `agb_allometries` in ./func.R. Returns a matrix of trees x simulations
mcTrees <- function(x, allom) {
  # Trees without a diameter can't be imputed if no other trees have a
  # diameter, so they are not included in AGB density
  n_tree <- nrow(x)
  if (all(is.na(x$diam_eq_cm))) {
    warning(n_tree, " trees without a diameter in plot ", x$plot_id[1], 
      " could not be imputed, as no other trees with the same allometry ",
      "have a diameter. They are not included in AGB density", call. = FALSE)
    return(matrix(NA_real_, nrow = n_tree, ncol = nsim))
  }

  # AGBmonteCarlo() fails with a single tree, so duplicate the row and keep
  # the first. Each row of `AGB_simu` is simulated independently
  if (n_tree == 1) {
    x <- x[c(1, 1), ]
  }

  # AGBmonteCarlo() warns where missing diameters are imputed. Stems with 
  # imputed diameters are summarised below. Allometries other than Chave et 
  # al. (2014) use the heights of the height-diameter model, if any
  out <- withCallingHandlers(AGBmonteCarlo(
    D = x$diam_eq_cm,
    WD = x$meanWD,
    errWD = x$sdWD,
    H = NULL,
    errH = NULL,
    HDmodel = if (param$height_method %in% c("field", "custom")) height_mod else NULL,
    coord = if (param$height_method == "regional" && allom == "chave2014") {
      as.matrix(x[, c("X", "Y")]) 
    } else NULL,
    Dpropag = diam_error,
    n = nsim,
    Carbon = FALSE,
    Dlim = NULL,
    fitted_allom = if (allom == "chave2014") {
      "chave2014"
    } else {
      agb_allometries[[allom]][c("formula", "parameters", "link")]
    }),
    warning = function(w) {
      if (grepl("NA values in D", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    })
  out$AGB_simu[seq_len(n_tree), , drop = FALSE]
}

# Run AGB Monte Carlo error propagation on each chunk, separately for each AGB
# allometry. Missing diameters are imputed from the other trees in the same
# chunk with the same allometry, i.e. the same plot, or the same group of
# quadrats for very large plots
tree_simu_list <- lapply(seq_along(tree_split), function(i) {
  x <- tree_split[[i]]
  message(i, " / ", length(tree_split), " - ", x$plot_id[1],
    " (", nrow(x), " trees)")
  out <- matrix(NA_real_, nrow = nrow(x), ncol = nsim)
  for (a in unique(x$agb_allometry)) {
    rows <- which(x$agb_allometry == a)
    out[rows, ] <- mcTrees(x[rows, ], a)
  }
  out
})

# Combine simulations, then share tree AGB among stems. Rows match `stem_fil`
tree <- bind_rows(tree_split)
tree_simu <- do.call(rbind, tree_simu_list)
stopifnot(nrow(tree_simu) == nrow(tree))
agb_simu <- tree_simu[match(stem_fil$tree_key, tree$tree_key), , drop = FALSE] *
  stem_fil$ba_share

# Trees where no stems have coordinates
unmapped <- !stem_fil$tree_mapped

# Stems below the minimum diameter threshold of the plot are used to estimate
# AGB, but are not included in AGB density
in_agbd <- stem_fil$record_id %in% record_agbd

# Calculate mean and standard deviation of stem AGB
# Stems used to estimate quadrat AGB density are those in a quadrat, and
# stems of trees without coordinates if they are placed at random. Stems
# without a diameter which could not be imputed are not included
agb_Mg_mean <- rowMeans(agb_simu, na.rm = TRUE)
# Trees without a diameter
tree_imp <- tree$tree_key[is.na(tree$diam_eq_cm)]
stem_agb_mc <- data.frame(
  record_id = stem_fil$record_id,
  agb_Mg_mean = agb_Mg_mean,
  agb_Mg_sd = sqrt(rowSums((agb_simu - agb_Mg_mean)^2, na.rm = TRUE) /
    (rowSums(!is.na(agb_simu)) - 1)),
  quadrat_id_tree = stem_fil$quadrat_id,
  diam_imputed = stem_fil$tree_key %in% tree_imp & is.finite(agb_Mg_mean),
  in_quadrat_calc = in_agbd & is.finite(agb_Mg_mean) & 
    ((!unmapped & !is.na(stem_fil$quadrat_id)) |
    (unmapped & param$unmapped_method == "random")))

# Write stem-level data to file
write.csv(stem_agb_mc, file.path(outdir, "stem_agb_mc.csv"), row.names = FALSE)

# Write stems with imputed diameters to file, to check raw data
unlink(file.path(report_dir, "diam_imputed.csv"))
if (any(stem_agb_mc$diam_imputed)) {
  diam_imputed <- stem_fil %>%
    filter(record_id %in% stem_agb_mc$record_id[stem_agb_mc$diam_imputed]) %>%
    mutate(agb_Mg_mean = agb_Mg_mean[match(record_id, stem_agb_mc$record_id)]) %>%
    dplyr::select(record_id, plot_id, tree_id, stem_id, taxon_name, code, 
      agb_Mg_mean) %>%
    arrange(plot_id, tree_id, stem_id)
  msgTable(count(diam_imputed, plot_id, name = "n_stem"), paste0(
    nrow(diam_imputed), " stems without a diameter, imputed from the other ",
    "trees in the same plot:"))
  write.csv(diam_imputed, file.path(report_dir, "diam_imputed.csv"), row.names = FALSE)
}

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

# Location of trees where no stems have coordinates, as bounds in the 
# relative coordinate system of the plot, from, in order of preference:
#   1. the bounds of the field subplot of the tree, in `subplot.csv`
#   2. the extent of the stems with coordinates in the same field subplot, if
#        there are at least `subplot_min_stem` of them, within the plot
#   3. the extent of the plot corners
subplot_min_stem <- 10
plot_ext <- plot_pt %>%
  filter(corner %in% TRUE, !is.na(x_rel_m), !is.na(y_rel_m)) %>%
  group_by(plot_id) %>%
  summarise(
    px_min = min(x_rel_m), px_max = max(x_rel_m),
    py_min = min(y_rel_m), py_max = max(y_rel_m),
    .groups = "drop")
sub_ext <- stem %>%
  filter(!is.na(subplot_id), subplot_id != "", !is.na(x_rel_m), !is.na(y_rel_m)) %>%
  group_by(plot_id, subplot_id) %>%
  summarise(
    n_stem_mapped = n(),
    sx_min = min(x_rel_m), sx_max = max(x_rel_m),
    sy_min = min(y_rel_m), sy_max = max(y_rel_m),
    .groups = "drop") %>%
  filter(n_stem_mapped >= subplot_min_stem)
if (!is.null(subplot)) {
  sub_tab <- dplyr::select(subplot, plot_id, subplot_id,
    tx_min = x_rel_min_m, tx_max = x_rel_max_m, 
    ty_min = y_rel_min_m, ty_max = y_rel_max_m)
} else {
  sub_tab <- data.frame(plot_id = character(0), subplot_id = character(0),
    tx_min = numeric(0), tx_max = numeric(0), 
    ty_min = numeric(0), ty_max = numeric(0))
}

# One row per tree where no stems have coordinates, with the field subplot
# of its first stem with a subplot, and the sum of the simulated AGB of its 
# stems
tree_un <- stem_fil %>%
  mutate(row = row_number()) %>%
  filter(unmapped, in_agbd) %>%
  group_by(plot_id, tree_id, tree_key) %>%
  summarise(
    stem_id = paste(stem_id, collapse = ", "),
    n_stem = n(),
    subplot_id = first(na.omit(na_if(subplot_id, ""))),
    diam_eq_cm = first(diam_eq_cm),
    rows = list(row),
    .groups = "drop") %>%
  left_join(., sub_tab, by = c("plot_id", "subplot_id")) %>%
  left_join(., sub_ext, by = c("plot_id", "subplot_id")) %>%
  left_join(., plot_ext, by = "plot_id") %>%
  mutate(
    location = case_when(
      !is.na(tx_min) ~ "field subplot, from subplot table",
      !is.na(sx_min) ~ "field subplot, from stems with coordinates",
      TRUE ~ "plot"),
    x_min = case_when(!is.na(tx_min) ~ tx_min, !is.na(sx_min) ~ pmax(sx_min, px_min), TRUE ~ px_min),
    x_max = case_when(!is.na(tx_max) ~ tx_max, !is.na(sx_max) ~ pmin(sx_max, px_max), TRUE ~ px_max),
    y_min = case_when(!is.na(ty_min) ~ ty_min, !is.na(sy_min) ~ pmax(sy_min, py_min), TRUE ~ py_min),
    y_max = case_when(!is.na(ty_max) ~ ty_max, !is.na(sy_max) ~ pmin(sy_max, py_max), TRUE ~ py_max))
tree_un_simu <- do.call(rbind, lapply(tree_un$rows, function(r) {
  colSums(agb_simu[r, , drop = FALSE])
}))
if (is.null(tree_un_simu)) {
  tree_un_simu <- matrix(numeric(0), nrow = 0, ncol = nsim)
}
tree_un$agb_Mg_mean <- rowMeans(tree_un_simu, na.rm = TRUE)

# Write trees without coordinates to file, to check raw data
tree_unmapped <- tree_un %>%
  dplyr::select(plot_id, tree_id, stem_id, n_stem, diam_eq_cm, agb_Mg_mean,
    subplot_id, location, x_min, x_max, y_min, y_max)
write.csv(tree_unmapped, file.path(report_dir, "tree_unmapped.csv"), row.names = FALSE)
if (nrow(tree_un) > 0) {
  msgTable(tree_un %>% 
    group_by(location) %>%
    summarise(n_tree = n(), agb_Mg = sum(agb_Mg_mean, na.rm = TRUE), .groups = "drop") %>%
    # Percentage of the AGB of all stems used to estimate AGB
    mutate(pct_agb = round(100 * agb_Mg / sum(stem_agb_mc$agb_Mg_mean, na.rm = TRUE), 2)),
    paste0(nrow(tree_un), " trees without coordinates, ", 
      if (param$unmapped_method == "random") "placed at random within:" else 
        "not included in AGB density (`unmapped_method` is \"exclude\"):"))
}

# Summarise trees without coordinates in each field subplot from the subplot
# table, as AGB per hectare of the subplot, to compare with quadrat AGB
# density
unlink(file.path(report_dir, "08_subplot_unmapped_map.png"))
tree_un_sub <- filter(tree_un, location == "field subplot, from subplot table")
if (!is.null(subplot) && nrow(tree_un_sub) > 0) {
  sub_un <- subplot %>%
    mutate(plot_id = as.character(plot_id)) %>%
    filter(plot_id %in% tree_un_sub$plot_id) %>%
    left_join(., tree_un_sub %>%
      group_by(plot_id, subplot_id) %>%
      summarise(n_tree = n(), agb_Mg = sum(agb_Mg_mean, na.rm = TRUE),
        .groups = "drop"),
      by = c("plot_id", "subplot_id")) %>%
    mutate(
      n_tree = ifelse(is.na(n_tree), 0L, n_tree),
      agb_Mg = ifelse(is.na(agb_Mg), 0, agb_Mg),
      area_ha = (x_rel_max_m - x_rel_min_m) * (y_rel_max_m - y_rel_min_m) / 10000,
      agbd_Mg_ha = agb_Mg / area_ha,
      x_mid = (x_rel_min_m + x_rel_max_m) / 2,
      y_mid = (y_rel_min_m + y_rel_max_m) / 2)
  msgTable(sub_un %>%
    filter(n_tree > 0) %>%
    arrange(desc(agbd_Mg_ha)) %>%
    transmute(plot_id, subplot_id, n_tree, agb_Mg, area_ha, agbd_Mg_ha),
    paste0("Field subplots with the most AGB in trees without coordinates ",
      "(Mg/ha of the subplot):"), max_rows = 10)

  # Figure of the AGB of trees without coordinates in each field subplot.
  # Labels are light on the darkest fills
  n_plot_un <- length(unique(sub_un$plot_id))
  un_lim <- c(0, max(c(sub_un$agbd_Mg_ha, 1e-6)))
  sub_un$label_colour <- ifelse(sub_un$agbd_Mg_ha < 0.6 * un_lim[2],
    fig_pal$ink, fig_pal$surface)
  p_sub_un <- ggplot(sub_un) +
    geom_blank(data = squareExtent(
      data.frame(plot_id = rep(sub_un$plot_id, 2),
        x = c(sub_un$x_rel_min_m, sub_un$x_rel_max_m),
        y = c(sub_un$y_rel_min_m, sub_un$y_rel_max_m)), "x", "y", "plot_id"),
      aes(x = x, y = y)) +
    geom_rect(aes(xmin = x_rel_min_m, xmax = x_rel_max_m, ymin = y_rel_min_m,
      ymax = y_rel_max_m, fill = agbd_Mg_ha),
      colour = fig_pal$surface, linewidth = 0.3) +
    scale_fill_gradientn("AGB of trees without coordinates (Mg/ha)",
      colours = fig_pal$seq2, limits = un_lim, guide = colourBar()) +
    geom_text(data = filter(sub_un, n_tree > 0),
      aes(x = x_mid, y = y_mid, label = n_tree, colour = label_colour),
      size = 3.2) +
    scale_colour_identity() +
    facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_un),
      scales = "free") +
    labs(
      title = "Trees without coordinates in each field subplot",
      subtitle = paste0("Field subplots from the subplot table, labelled with ",
        "the number of trees without coordinates, which are placed at random ",
        "within their subplot. Simulated mean AGB per hectare of the subplot. ",
        "Subplots without labels have no trees without coordinates. ",
        "Trees without a field subplot are not shown"),
      x = "X (m)",
      y = "Y (m)") +
    themeMap() +
    theme(aspect.ratio = 1)
  saveFig(p_sub_un, file.path(report_dir, "08_subplot_unmapped_map.png"),
    n_panel = n_plot_un)
}

# Aggregate simulations to quadrats
# subplot_summary() is run separately for each plot, as it assigns the wrong
# plot IDs to quadrats when filling empty quadrats if plots have different
# numbers of quadrats
# Plots with no stems used to estimate AGB, e.g. plots with no trees above
# the minimum diameter threshold, have quadrats with zero AGB density
# Trees without coordinates are placed at random within their bounds in each
# simulation, see `tree_bounds` in BIOMASS::subplot_summary(). Placements 
# outside the quadrat grid are not included in that simulation
plot_vec <- unique(quad_pt_clean$plot_ID)
bound_cols <- c("x_min", "x_max", "y_min", "y_max")

quad_agb_simu <- bind_rows(lapply(plot_vec, function(x) {
  quads <- quad_area[quad_area$plot_ID == x, ]

  # Stems with coordinates, and trees without coordinates
  sel <- stem_fil$plot_id == x & !unmapped & in_agbd
  sel_un <- if (param$unmapped_method == "random") which(tree_un$plot_id == x) else integer(0)
  if (any(sel) || length(sel_un) > 0) {
    tree_data <- data.frame(
      plot_ID = x,
      subplot_ID = c(stem_fil$quadrat_id[sel], rep(NA_character_, length(sel_un))))
    tree_data[bound_cols] <- rbind(
      matrix(NA_real_, nrow = sum(sel), ncol = 4),
      as.matrix(tree_un[sel_un, bound_cols]))
    subplots <- list(
      sub_corner_coord = quad_pt_clean[quad_pt_clean$plot_ID == x, ],
      tree_data = tree_data)

    ss <- suppressMessages(subplot_summary(
      subplots = subplots,
      AGB_simu = rbind(agb_simu[sel, , drop = FALSE], tree_un_simu[sel_un, , drop = FALSE]),
      draw_plot = FALSE,
      tree_bounds = if (length(sel_un) > 0) bound_cols else NULL))

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

  out$plot_id <- x
  out
}))

# Crown-distributed AGB density, see `crown_method` in param.yaml. In each
# simulation, the AGB of each tree is spread over its crown, a circle centred
# on the centroid of its stems, with area from the crown area allometry in
# `crown_custom`. The residual error of the allometry is drawn independently
# for each tree in each simulation. Crown area is predicted from the measured
# diameter, the equivalent diameter of the measured diameters of multi-stemmed
# trees, as crown allometries are fitted to measured diameters, so diameters
# are not corrected for taper. Crown area is not predicted from the simulated
# diameter, as BIOMASS::AGBmonteCarlo() does not return simulated diameters.
# Crowns crossing a quadrat edge are split among
# quadrats, see `crownAlloc()` in ./func.R. The part of a crown outside the
# quadrat grid is folded back into the part inside, so plot AGB density is
# the same as with each tree in its home quadrat, and only its distribution
# among quadrats changes. Trees without coordinates, placed at random, and
# trees with an imputed diameter keep the stem allocation. Crown simulations
# are drawn after the simulations above, so the stem allocation is the same
# whether or not crowns are used
if (param$crown_method != "none") {
  crown_mod <- crownCustom(param$crown_custom)
  crown_tmpl <- crownTemplate(n_ring = 16, n_angle = 32)
  quad_rel <- st_drop_geometry(quad_pt)

  # Stems without coordinates in trees where other stems have coordinates
  # are given the mean coordinates of the other stems of the tree, as in
  # ./07_quad.R, where trees are assigned to quadrats
  xy_fill <- stem %>%
    mutate(tree_grp = ifelse(is.na(tree_id) | tree_id == "",
      paste0("record::", record_id), paste(plot_id, tree_id, sep = "::"))) %>%
    group_by(tree_grp) %>%
    mutate(
      xy_ok = !is.na(x_rel_m) & !is.na(y_rel_m),
      x_rel_m = ifelse(!xy_ok & any(xy_ok), mean(x_rel_m[xy_ok]), x_rel_m),
      y_rel_m = ifelse(!xy_ok & any(xy_ok), mean(y_rel_m[xy_ok]), y_rel_m)) %>%
    ungroup()
  xy_i <- match(stem_fil$record_id, xy_fill$record_id)

  crown_list <- lapply(plot_vec, function(x) {
    grid <- quadGrid(quad_rel[quad_rel$plot_id == x, ])
    # Stems with coordinates in a quadrat, as in the stem allocation above
    rows <- which(stem_fil$plot_id == x & !unmapped & in_agbd &
      !is.na(stem_fil$quadrat_id))
    if (is.null(grid)) {
      warning("Quadrats in plot ", x, " are not a regular grid, crown ",
        "allocation is not used", call. = FALSE)
      return(NULL)
    }
    if (length(rows) == 0) {
      return(NULL)
    }

    # One row per tree, at the centroid of its stems, as in ./07_quad.R, with
    # the equivalent diameter of the measured diameters of its stems
    s <- stem_fil[rows, ]
    s$x_rel_m <- xy_fill$x_rel_m[xy_i[rows]]
    s$y_rel_m <- xy_fill$y_rel_m[xy_i[rows]]
    tr <- s %>%
      group_by(tree_key) %>%
      summarise(
        x = mean(x_rel_m, na.rm = TRUE),
        y = mean(y_rel_m, na.rm = TRUE),
        quadrat_id = first(quadrat_id),
        diam_crown_cm = sqrt(sum(diam_cm_meas^2)),
        .groups = "drop") %>%
      mutate(home = match(quadrat_id, grid$quad$quadrat_id))
    stopifnot(all(!is.na(tr$home)))
    agb_s <- agb_simu[rows, , drop = FALSE]
    agb_s[is.na(agb_s)] <- 0
    agb_t <- rowsum(agb_s, s$tree_key)[tr$tree_key, , drop = FALSE]

    # Trees with an imputed diameter, or without coordinates, keep stem
    # allocation, as a crown of radius 0 at the centre of the home quadrat
    has_crown <- is.finite(tr$diam_crown_cm) & is.finite(tr$x) & is.finite(tr$y)
    hq <- grid$quad[tr$home, ]
    tr$x[!has_crown] <- (hq$x_min[!has_crown] + hq$x_max[!has_crown]) / 2
    tr$y[!has_crown] <- (hq$y_min[!has_crown] + hq$y_max[!has_crown]) / 2
    d_edge <- pmin(tr$x - hq$x_min, hq$x_max - tr$x, tr$y - hq$y_min,
      hq$y_max - tr$y)

    # Difference in AGB of each quadrat between crown and stem allocation, in
    # each simulation
    nq <- nrow(grid$quad)
    n_cross <- numeric(nsim)
    delta <- vapply(seq_len(nsim), function(j) {
      r <- numeric(nrow(tr))
      r[has_crown] <- sqrt(crownArea(crown_mod, tr$diam_crown_cm[has_crown],
        rnorm(sum(has_crown))) / pi)
      n_cross[j] <<- sum(r > d_edge)
      crown_q <- crownAlloc(tr$x, tr$y, agb_t[, j], r, tr$home, grid, crown_tmpl)
      stem_q <- numeric(nq)
      s_q <- rowsum(agb_t[, j], tr$home)
      stem_q[as.integer(rownames(s_q))] <- s_q[, 1]
      crown_q - stem_q
    }, numeric(nq))

    quads <- quad_area[quad_area$plot_ID == x, ]
    area_ha <- quads$area_ha[match(grid$quad$quadrat_id, quads$subplot_ID)]
    list(
      # Trees with a crown, with the median crown radius and mean simulated
      # AGB, for the figure of crowns
      tree = data.frame(
        plot_id = x,
        x = tr$x[has_crown],
        y = tr$y[has_crown],
        r = sqrt(crownArea(crown_mod, tr$diam_crown_cm[has_crown]) / pi),
        agb_Mg = rowMeans(agb_t[has_crown, , drop = FALSE])),
      delta = data.frame(
        plot_id = x,
        quadrat_id = rep(grid$quad$quadrat_id, nsim),
        sim = rep(seq_len(nsim), each = nq),
        agbd_diff = as.vector(delta / area_ha)),
      summ = data.frame(
        plot_id = x,
        n_tree = nrow(tr),
        n_tree_crown = sum(has_crown),
        crown_area_m2_median = median(crownArea(crown_mod,
          tr$diam_crown_cm[has_crown])),
        pct_tree_cross = round(100 * mean(n_cross) / nrow(tr), 1)))
  })

  crown_list <- Filter(Negate(is.null), crown_list)
  crown_delta <- bind_rows(lapply(crown_list, `[[`, "delta"),
    data.frame(plot_id = character(0), quadrat_id = character(0),
      sim = integer(0), agbd_diff = numeric(0)))
  quad_agb_simu <- quad_agb_simu %>%
    left_join(., crown_delta, by = c("plot_id", "quadrat_id", "sim")) %>%
    mutate(agbd_Mg_ha_crown = agbd_Mg_ha + coalesce(agbd_diff, 0)) %>%
    dplyr::select(-agbd_diff)

  # Crowns are folded back into the quadrat grid, so the AGB of each plot
  # should be the same with crown and stem allocation in every simulation
  crown_check <- quad_agb_simu %>%
    inner_join(., transmute(quad_area, plot_id = as.character(plot_ID),
      quadrat_id = subplot_ID, area_ha), by = c("plot_id", "quadrat_id")) %>%
    group_by(plot_id, sim) %>%
    summarise(
      agb_stem = sum(agbd_Mg_ha * area_ha),
      agb_crown = sum(agbd_Mg_ha_crown * area_ha),
      .groups = "drop")
  crown_bad <- abs(crown_check$agb_crown - crown_check$agb_stem) >
    1e-6 * pmax(abs(crown_check$agb_stem), 1)
  if (any(crown_bad, na.rm = TRUE)) {
    warning("Plot AGB differs between crown and stem allocation in plots: ",
      paste(unique(crown_check$plot_id[crown_bad %in% TRUE]), collapse = ", "),
      call. = FALSE)
  }

  msgTable(bind_rows(lapply(crown_list, `[[`, "summ")), paste0(
    "Crown allocation, ", crown_mod$reference, ". Trees with coordinates in ",
    "a quadrat, trees with a crown (others keep stem allocation), median ",
    "crown area (m2), and mean percentage of trees whose crown crosses a ",
    "quadrat edge:"))
}

# Write quadrat simulations to file
write.csv(quad_agb_simu, file.path(outdir, "quad_agb_mc.csv"), row.names = FALSE)

# Extract summary statistics from AGBD simulations. With crown allocation,
# also `agbd_Mg_ha_crown_mc_*`
quad_agb <- quad_agb_simu %>%
  group_by(plot_id, quadrat_id) %>%
  summarise(
    agbd_Mg_ha_mc_mean = mean(agbd_Mg_ha),
    agbd_Mg_ha_mc_median = median(agbd_Mg_ha),
    agbd_Mg_ha_mc_sd = sd(agbd_Mg_ha),
    agbd_Mg_ha_mc_ci2.5 = quantile(agbd_Mg_ha, 0.025),
    agbd_Mg_ha_mc_ci97.5 = quantile(agbd_Mg_ha, 0.975),
    across(any_of("agbd_Mg_ha_crown"), list(
      mc_mean = mean,
      mc_median = median,
      mc_sd = sd,
      mc_ci2.5 = ~quantile(.x, 0.025),
      mc_ci97.5 = ~quantile(.x, 0.975))),
    .groups = "drop")

# Write quadrat-level summary data to file
write.csv(quad_agb, file.path(outdir, "quad_agb.csv"), row.names = FALSE)

# Figure of the difference in quadrat AGB density between crown and stem
# allocation, simulated means
unlink(file.path(report_dir, "08_quad_agbd_crown_diff_map.png"))
if (param$crown_method != "none") {
  crown_fig <- quad_rel %>%
    group_by(plot_id, quadrat_id) %>%
    summarise(
      x_min = min(x_rel_m), x_max = max(x_rel_m),
      y_min = min(y_rel_m), y_max = max(y_rel_m),
      .groups = "drop") %>%
    inner_join(., quad_agb, by = c("plot_id", "quadrat_id")) %>%
    mutate(agbd_diff = agbd_Mg_ha_crown_mc_mean - agbd_Mg_ha_mc_mean)
  n_plot_crown <- length(unique(crown_fig$plot_id))
  diff_lim <- c(-1, 1) * max(abs(crown_fig$agbd_diff), 1e-6)
  p_crown <- ggplot(crown_fig) +
    geom_blank(data = squareExtent(
      data.frame(plot_id = rep(crown_fig$plot_id, 2),
        x = c(crown_fig$x_min, crown_fig$x_max),
        y = c(crown_fig$y_min, crown_fig$y_max)), "x", "y", "plot_id"),
      aes(x = x, y = y)) +
    geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max,
      fill = agbd_diff), colour = fig_pal$surface, linewidth = 0.3) +
    scale_fill_gradient2("Crown minus stem allocation (Mg/ha)",
      low = fig_pal$series[1], mid = fig_pal$surface, high = fig_pal$series[2],
      midpoint = 0, limits = diff_lim, guide = colourBar()) +
    facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_crown),
      scales = "free") +
    labs(
      title = "Quadrat AGBD, crown minus stem allocation",
      subtitle = paste0("Simulated means. Orange quadrats gain AGB from ",
        "crowns of trees in neighbouring quadrats, blue quadrats lose AGB ",
        "to them. Crown area: ", crown_mod$reference),
      x = "X (m)",
      y = "Y (m)") +
    themeMap() +
    theme(aspect.ratio = 1)
  saveFig(p_crown, file.path(report_dir, "08_quad_agbd_crown_diff_map.png"),
    n_panel = n_plot_crown)
}

# Figure of tree crowns in each plot, as circles with the median crown area
# from the crown area allometry, filled by the simulated mean AGB of the tree.
# Larger crowns are drawn first, so smaller crowns stay visible
unlink(file.path(report_dir, "08_crown_map.png"))
if (param$crown_method != "none") {
  crown_tree <- bind_rows(lapply(crown_list, `[[`, "tree")) %>%
    filter(agb_Mg > 0) %>%
    arrange(desc(r)) %>%
    mutate(tree = row_number())
  if (nrow(crown_tree) > 0) {
    circ_n <- 24
    circ_a <- 2 * pi * seq_len(circ_n) / circ_n
    crown_poly <- data.frame(
      plot_id = rep(crown_tree$plot_id, each = circ_n),
      tree = rep(crown_tree$tree, each = circ_n),
      agb_Mg = rep(crown_tree$agb_Mg, each = circ_n),
      x = rep(crown_tree$x, each = circ_n) +
        as.vector(outer(cos(circ_a), crown_tree$r)),
      y = rep(crown_tree$y, each = circ_n) +
        as.vector(outer(sin(circ_a), crown_tree$r)))
    n_plot_crown <- length(unique(crown_tree$plot_id))
    p_crown_map <- ggplot() +
      geom_blank(data = squareExtent(
        data.frame(plot_id = rep(crown_fig$plot_id, 2),
          x = c(crown_fig$x_min, crown_fig$x_max),
          y = c(crown_fig$y_min, crown_fig$y_max)), "x", "y", "plot_id"),
        aes(x = x, y = y)) +
      geom_polygon(data = crown_poly, aes(x = x, y = y, group = tree,
        fill = agb_Mg), alpha = 0.4, colour = NA) +
      geom_rect(data = crown_fig, aes(xmin = x_min, xmax = x_max,
        ymin = y_min, ymax = y_max), fill = NA, colour = fig_pal$ink_muted,
        linewidth = 0.3) +
      # The lightest colour of the ramp is not used, so that crowns of trees
      # with little AGB stay visible
      scale_fill_gradientn("Tree AGB (Mg)", colours = fig_pal$seq2[-1],
        trans = "log10", breaks = 10^(-3:3),
        labels = function(x) format(x, drop0trailing = TRUE,
          scientific = FALSE, trim = TRUE),
        guide = colourBar()) +
      facet_wrap(vars(plotFactor(plot_id)), ncol = figCols(n_plot_crown),
        scales = "free") +
      labs(
        title = "Tree crowns in each plot",
        subtitle = paste0("Circles with the median crown area from ",
          crown_mod$reference, ", filled by simulated mean tree AGB, with ",
          param$quad_dim[1], " x ", param$quad_dim[2], " m quadrats. Trees ",
          "with coordinates in a quadrat"),
        x = "X (m)",
        y = "Y (m)") +
      themeMap() +
      theme(aspect.ratio = 1)
    saveFig(p_crown_map, file.path(report_dir, "08_crown_map.png"),
      n_panel = n_plot_crown)
  }
}

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

# Stems in strips of the plot left out of the quadrat grid are excluded, as
# the quadrat grid area does not include them
plot_agb_det <- stem_fil %>%
  filter(as.character(record_id) %in% record_agbd, !grid_excl) %>%
  dplyr::select(record_id, plot_id) %>%
  inner_join(., stem_agb, by = "record_id") %>%
  mutate(plot_id = as.character(plot_id)) %>%
  group_by(plot_id) %>%
  summarise(agb_Mg = sum(agb_Mg, na.rm = TRUE), .groups = "drop") %>%
  right_join(., quad_area_join %>% group_by(plot_id) %>% 
    summarise(area_ha = sum(area_ha), n_quadrat = n(), .groups = "drop"),
    by = "plot_id") %>%
  mutate(
    agb_Mg = ifelse(is.na(agb_Mg), 0, agb_Mg),
    agbd_Mg_ha_stem_sum = agb_Mg / area_ha)

plot_agb_check <- plot_agb_det %>%
  dplyr::select(plot_id, n_quadrat, area_ha, agbd_Mg_ha_stem_sum) %>%
  inner_join(., plot_agb_mc, by = "plot_id") %>%
  mutate(
    # Undefined for plots with zero AGB density
    cv_pct = ifelse(agbd_Mg_ha_mc_mean > 0, 
      round(100 * agbd_Mg_ha_mc_sd / agbd_Mg_ha_mc_mean, 1), NA_real_),
    diff_pct = ifelse(agbd_Mg_ha_stem_sum > 0, 
      round(100 * (agbd_Mg_ha_mc_mean - agbd_Mg_ha_stem_sum) / 
        agbd_Mg_ha_stem_sum, 1), NA_real_)) %>%
  arrange(desc(agbd_Mg_ha_mc_mean))
msgTable(plot_agb_check, paste0("Plot AGB density (Mg/ha) over the quadrat ",
  "grid, from ", nsim, " simulations (mc), and the sum of stem AGB ",
  "(stem_sum). diff_pct is the difference between them:"))
write.csv(plot_agb_check, file.path(outdir, "plot_agb.csv"), row.names = FALSE)

if (any(abs(plot_agb_check$diff_pct) > 10, na.rm = TRUE)) {
  warning("Simulated plot AGB density differs from the sum of stem AGB by ",
    "more than 10% in plots: ", 
    paste(plot_agb_check$plot_id[abs(plot_agb_check$diff_pct) > 10], collapse = ", "),
    ". Check stems outside the quadrat grid, see ", 
    file.path(report_dir, "stem_outside.csv"), call. = FALSE)
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
    title = "Plot AGBD",
    subtitle = paste0("Over the quadrat grid of each plot. ", nsim, 
      " simulations, diameter error: ", 
      if (is.list(diam_error)) "field" else format(param$diam_error)),
    x = "AGBD (Mg/ha)",
    y = "Plot") +
  themeFig()
saveFig(p_plot_agb, file.path(report_dir, "08_plot_agbd.png"), width = 8,
  height = min(max(2.6 + 0.35 * nrow(plot_agb_fig), 4), 24))
