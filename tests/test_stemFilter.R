# Tests for stem selection functions in ./func.R
# Run from the repository root: testthat::test_file("./tests/test_stemFilter.R")

library(testthat)
library(dplyr)

# Repository root, whether run from the root or from ./tests/
root <- if (file.exists("func.R")) "." else ".."

source(file.path(root, "func.R"))

plot <- data.frame(plot_id = c("P1", "P2"), meas_diam_min_cm = c(10, 5))

stem <- data.frame(
  record_id = 1:7,
  site_id = "S1",
  plot_id = c("P1", "P1", "P1", "P2", "P2", "P2", "P2"),
  tree_id = c("1", "2", "3", "4", "5", "5", "6"),
  stem_id = c("1", "2", "3", "4", "5a", "5b", "6"),
  pom_m = 1.3,
  measurement_date = 2024.5,
  diam_cm = c(12, 8, 30, 6, 20, 15, 25),
  code = c("AS", "AS", "ASB", "AS", "AS", "AS", "DS"))

test_that("recordFilter() keeps stems with a diameter, including dead stems", {
  expect_setequal(recordFilter(stem, plot), as.character(1:7))
})

test_that("agbdFilter() uses the minimum diameter threshold of each plot", {
  record_fil <- recordFilter(stem, plot)
  expect_setequal(agbdFilter(stem, plot, record_fil), c("1", "3", "4", "5", "6"))
})

test_that("agbdFilter() uses the larger of `diam_min_cm` and the plot threshold", {
  record_fil <- recordFilter(stem, plot)
  expect_setequal(agbdFilter(stem, plot, record_fil, diam_min_cm = 7),
    c("1", "3", "5", "6"))
  expect_setequal(agbdFilter(stem, plot, record_fil, diam_min_cm = 16),
    c("3", "5"))
})

test_that("agbdFilter() excludes stems broken above the POM if requested", {
  record_fil <- recordFilter(stem, plot)
  expect_setequal(agbdFilter(stem, plot, record_fil, include_broken = FALSE),
    c("1", "4", "5", "6"))
})

test_that("recordFilter() optionally keeps stems without a diameter", {
  stem_na <- rbind(stem, data.frame(record_id = 8:10, site_id = "S1", 
    plot_id = "P1", tree_id = c("7", "8", "8"), stem_id = c("7", "8", "8"), 
    pom_m = 1.3, measurement_date = c(2024.5, 2024.5, 2024.6), 
    diam_cm = c(NA, 14, NA), code = c("ABS", "AS", "AS")))
  # Without a diameter: excluded by default
  expect_setequal(recordFilter(stem_na, plot), as.character(c(1:7, 9)))
  # Included, but a repeat record with a diameter is preferred
  expect_setequal(recordFilter(stem_na, plot, missing_diam = TRUE), 
    as.character(c(1:7, 8, 9)))
  # Stems without a diameter are assumed to be above the threshold
  record_fil <- recordFilter(stem_na, plot, missing_diam = TRUE)
  expect_true("8" %in% agbdFilter(stem_na, plot, record_fil))
  expect_false("8" %in% agbdFilter(stem_na, plot, record_fil, include_broken = FALSE))
})

test_that("dead and fallen stems with a diameter have AGB, but not AGB density", {
  stem_df <- rbind(stem, data.frame(record_id = 8:14, site_id = "S1",
    plot_id = "P1", tree_id = as.character(7:13), stem_id = as.character(7:13),
    pom_m = 1.3, measurement_date = 2024.5,
    diam_cm = c(22, 18, 16, NA, NA, 40, 30),
    code = c("AF", "DF", "D", "DS", "AF", "DST", NA)))
  # Dead standing (7), alive fallen (8), dead fallen (9) and dead (10) stems
  # with a diameter. Not without a diameter (11, 12), broken below the POM
  # (13) or without a code (14)
  expect_setequal(recordFilter(stem_df, plot), as.character(1:10))
  # Diameters of dead or fallen stems are never imputed
  expect_setequal(recordFilter(stem_df, plot, missing_diam = TRUE),
    as.character(1:10))
  # None of them are included in AGB density
  expect_setequal(agbdFilter(stem_df, plot, recordFilter(stem_df, plot)),
    c("1", "3", "4", "5", "6"))
})

test_that("treeEquiv() combines only included stems into trees", {
  te <- treeEquiv(stem, c("5", "6"))
  te <- te[match(c("5", "6"), te$record_id), ]
  expect_equal(te$tree_key[1], te$tree_key[2])
  expect_equal(te$diam_eq_cm[1], sqrt(20^2 + 15^2))
  expect_equal(sum(te$ba_share), 1)

  # No stems included: each stem is its own tree
  te0 <- treeEquiv(stem, character(0))
  expect_equal(nrow(te0), nrow(stem))
  expect_true(all(te0$ba_share == 1))
  expect_equal(te0$diam_eq_cm[match(c(5, 6), te0$record_id)], c(20, 15))
})

test_that("agbdFilter() excludes stems of growth forms in `growth_form_exclude`", {
  stem_gf <- mutate(stem, growth_form = c("tree", "tree", "strangler", "palm",
    "tree", "tree", "tree"))
  record_fil <- recordFilter(stem_gf, plot)
  expect_setequal(agbdFilter(stem_gf, plot, record_fil,
    growth_form_exclude = "strangler"), c("1", "4", "5", "6"))
  expect_setequal(agbdFilter(stem_gf, plot, record_fil,
    growth_form_exclude = c("strangler", "palm")), c("1", "5", "6"))
})

test_that("agbdFilter() applies the diameter threshold to the measured diameter", {
  # Stem 2 is 8 cm measured, 11 cm after taper correction, in a 10 cm plot
  stem_taper <- mutate(stem, diam_cm_meas = diam_cm,
    diam_cm = ifelse(record_id == 2, 11, diam_cm))
  record_fil <- recordFilter(stem_taper, plot)
  expect_false("2" %in% agbdFilter(stem_taper, plot, record_fil))
})
