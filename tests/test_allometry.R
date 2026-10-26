# Tests for taper correction, AGB and height allometry, and parameter
# functions in ./func.R
# Run from the repository root: testthat::test_file("./tests/test_allometry.R")

library(testthat)
suppressPackageStartupMessages(library(dplyr))

# Repository root, whether run from the root or from ./tests/
root <- if (file.exists("func.R")) "." else ".."

source(file.path(root, "func.R"))

test_that("taperB() gives the Cushman et al. (2014) taper parameter", {
  expect_equal(taperB(50, 3, "cushman2014"),
    exp(-2.0205 - 0.5053 * log(50) + 0.3748 * log(3)))
  expect_equal(taperB(50, 3, "cushman2014", sp_term = 0.4865),
    exp(-2.0205 + 0.4865 - 0.5053 * log(50) + 0.3748 * log(3)))
})

test_that("taperB() gives the Cushman et al. (2021) taper parameter, at least 0", {
  expect_equal(taperB(50, 3, "cushman2021", wd = 0.6),
    0.151 - 0.025 * log(50) - 0.02 * log(3) - 0.021 * log(0.6))
  expect_equal(taperB(300, 10, "cushman2021", wd = 0.6), 0)
  expect_error(taperB(50, 3, "none"), "Unknown taper method")
})

test_that("taperDiam() replaces diameters with taper-corrected diameters", {
  stem <- data.frame(record_id = 1:3, diam_cm = c(50, 20, NA))
  stem_height <- data.frame(record_id = c(1, 2, 3),
    diam_cm_taper = c(55, NA, NA))
  out <- taperDiam(stem, stem_height)
  expect_equal(out$diam_cm, c(55, 20, NA))
  expect_equal(out$diam_cm_meas, c(50, 20, NA))
  # Without taper correction, diameters are unchanged
  out <- taperDiam(stem, stem_height[, "record_id", drop = FALSE])
  expect_equal(out$diam_cm, stem$diam_cm)
  expect_equal(out$diam_cm_meas, stem$diam_cm)
})

test_that("agbAllomMean() gives the mean of the Goodman et al. (2013) palm allometry", {
  D <- c(10, 25)
  expect_equal(agbAllomMean(agb_allometries$goodman2013, D = D),
    exp(-3.3488 + 2.7483 * log(D) + 0.5876^2 / 2) / 1000)
})

test_that("heightCustom() builds a height-diameter model with BIOMASS::customHD()", {
  skip_if_not(exists("customHD", envir = asNamespace("BIOMASS")),
    "BIOMASS without customHD()")
  mod <- heightCustom(list(formula = "a * D^b / (k + D^b)",
    parameters = list(a = 57.1, b = 0.73, k = 21.79), sigma = 0.181,
    link = "log", reference = "Test"))
  D <- c(10, 50)
  median_h <- 57.1 * D^0.73 / (21.79 + D^0.73)
  expect_equal(BIOMASS::retrieveH(D = D, model = mod)$H,
    median_h * exp(0.181^2 / 2))
})

test_that("paramString() collapses parameters with several or named values", {
  expect_equal(paramString("regional"), "regional")
  expect_equal(paramString(10), 10)
  expect_equal(paramString(c("strangler", "liana")), "strangler, liana")
  expect_equal(paramString(list(a = 0.57, b = 1.34)), "a = 0.57; b = 1.34")
  expect_equal(paramString(list(formula = "a * D^b",
    parameters = list(a = 0.57, b = 1.34))),
    "formula = a * D^b; parameters.a = 0.57; parameters.b = 1.34")
})
