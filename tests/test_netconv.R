# Tests for functions used to convert plot network data in ./func.R
# Run from the repository root: testthat::test_file("./tests/test_netconv.R")

library(testthat)
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

# Repository root, whether run from the root or from ./tests/
root <- if (file.exists("func.R")) "." else ".."

source(file.path(root, "func.R"))
pt_cols <- read.csv(file.path(root, "templates/pt_cols.csv"))

test_that("dateRes() identifies date resolution", {
  expect_equal(dateRes(c("2024-08-03", "2024-08", "2024", "03/08/2024", NA, " 2024 ")),
    c("YYYY-MM-DD", "YYYY-MM", "YYYY", NA, NA, "YYYY"))
})

test_that("codeMap() translates flags", {
  # Multi-character flags with a separator
  map <- c(Q = "B", X = "T", DC = "DF", DS = "DS", L = "")
  expect_equal(codeMap(c("Q;L", "DC", "", NA, "X; DS"), map, split = "[;, ]+"),
    c("B", "DF", NA, NA, "TDS"))

  # Single character flags
  expect_equal(codeMap(c("bf", "s", "fz"), c(b = "B", f = "F", s = "S", z = ""), split = ""),
    c("BF", "S", "F"))

  # Whole values as flags
  expect_equal(codeMap(c("alive", "broken below", NA), c(alive = "A", "broken below" = "AT"), split = NA),
    c("A", "AT", NA))

  # Unknown flags give a warning
  expect_warning(codeMap("QZ", c(Q = "B")), "Flags not in `map`, ignored: Z")
})

test_that("codeCombine() creates valid codes", {
  expect_equal(codeCombine(c("A", "A", "D", "A", "D", NA), c("BS", NA, "F", "TB", NA, NA)),
    c("ASB", "A", "DF", "AT", "D", NA))

  # Fallen replaces standing, missing removes measurement codes
  expect_equal(codeCombine("A", "SF"), "AF")
  expect_equal(codeCombine("D", "MSB"), "DM")

  # Standing added to stems which are not fallen or missing
  expect_equal(codeCombine(c("A", "A", "D", "D"), c(NA, "F", "M", NA), standing = TRUE),
    c("AS", "AF", "DM", "DS"))
  expect_equal(codeCombine(c("A", "D"), standing = c(TRUE, FALSE)), c("AS", "D"))

  # Codes are ordered as in ./templates/stem_codes.csv
  expect_equal(codeCombine("HIUBSA"), "ASBUIH")
})

test_that("ptFormat() creates plot geo-location table", {
  x <- data.frame(
    site_id = "Site1", plot_id = "P1", point_id = c("SW", "SE", "NE", "NW"),
    x_rel_m = c(0, 100, 100, 0), y_rel_m = c(0, 0, 100, 100),
    longitude = c(15.0, 15.0009, 15.0009, 15.0), 
    latitude = c(-15.0, -15.0, -14.9991, -14.9991))
  pt <- ptFormat(x, pt_cols)

  expect_equal(names(pt), pt_cols$column_name)
  expect_silent(colCheck(pt, pt_cols))
  expect_equal(unique(pt$crs_epsg), 32733L)
  expect_equal(unique(pt$crs_name), "UTM 33S")
  expect_true(all(pt$corner))
  # Plot is approximately 100 m wide
  expect_equal(diff(pt$rover_easting_utm_m[1:2]), 96.7, tolerance = 0.01)

  # Projected coordinates are kept
  y <- x[, 1:5]
  y$rover_easting_utm_m <- 500000 + y$x_rel_m
  y$rover_northing_utm_m <- 8000000 + y$y_rel_m
  y$crs_epsg <- 32733
  pt2 <- ptFormat(y, pt_cols)
  expect_equal(pt2$rover_easting_utm_m, y$rover_easting_utm_m)

  expect_error(ptFormat(x[, 1:5], pt_cols), "must contain either")

  # Columns are converted to the classes in the template
  z <- x
  z$x_rel_m <- as.integer(z$x_rel_m)
  z$point_id <- factor(z$point_id)
  expect_silent(colCheck(ptFormat(z, pt_cols), pt_cols))
})

test_that("recordFilter() selects stems for AGB density", {
  plot <- data.frame(plot_id = "P1", meas_diam_min_cm = 10)
  stem <- data.frame(
    record_id = 1:8, site_id = "S", plot_id = "P1",
    tree_id = c("T1", "T1", "T2", "T3", "T4", "T5", "T6", "T6"),
    stem_id = c("a", "b", "c", "d", "e", "f", "g", "g"),
    diam_cm = c(20, 15, 5, 30, 25, NA, 40, 42),
    pom_m = c(1.3, 1.3, 1.3, 1.3, 1.3, NA, 1.3, 2),
    measurement_date = 2024.5,
    code = c("AS", "ASB", "AS", "DS", "AST", "ASM", "AS", "AS"))
  # Excludes small (3), dead (4), broken below POM (5), missing (6), and
  # the lower POM of a stem measured twice (7). Broken above POM (2) included
  expect_equal(recordFilter(stem, plot), c("1", "2", "8"))
})

test_that("treeEquiv() calculates equivalent diameters", {
  stem <- data.frame(
    record_id = 1:6, plot_id = c("P1", "P1", "P1", "P2", "P1", "P1"),
    tree_id = c("T1", "T1", "T2", "T1", NA, "T1"),
    diam_cm = c(30, 40, 20, 10, 15, 50))
  # Stem 6 is excluded, e.g. dead
  eq <- treeEquiv(stem, include = 1:5)

  expect_equal(eq$diam_eq_cm, c(50, 50, 20, 10, 15, 50))
  expect_equal(eq$ba_share, c(0.36, 0.64, 1, 1, 1, 1))
  expect_equal(eq$n_stem_tree, c(2, 2, 1, 1, 1, 1))
  # Same tree_id in different plots are different trees
  expect_false(eq$tree_key[4] == eq$tree_key[1])
  # Basal area is preserved
  expect_equal(sum(eq$diam_eq_cm[1:2]^2 * eq$ba_share[1:2]) / 1, 50^2)
})
