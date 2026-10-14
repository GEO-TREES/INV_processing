# Tests for L0 data validation functions in ./func.R
# Run from the repository root: testthat::test_file("./tests/test_valCheck.R")

library(testthat)

# Repository root, whether run from the root or from ./tests/
root <- if (file.exists("func.R")) "." else ".."

source(file.path(root, "func.R"))

stem_cols <- read.csv(file.path(root, "templates/stem_cols.csv"))
plot_cols <- read.csv(file.path(root, "templates/plot_cols.csv"))
pt_cols <- read.csv(file.path(root, "templates/pt_cols.csv"))
height_cols <- read.csv(file.path(root, "templates/height_cols.csv"))
cols <- list(stem = stem_cols, plot = plot_cols, pt = pt_cols, height = height_cols)

# Create a small valid set of L0 tables, with columns from the templates
emptyTable <- function(spec, n) {
  out <- colGen(data.frame(row.names = seq_len(n)), spec$column_name, spec$class)
  out[, spec$column_name]
}

makeL0 <- function() {
  plot <- suppressMessages(emptyTable(plot_cols, 2))
  plot$site_id <- "Site1"
  plot$acquisition_id <- "2024-08-03"
  plot$plot_id <- c("P1", "P2")
  plot$plot_planar <- TRUE
  plot$plot_length_m <- 100
  plot$plot_width_m <- 100
  plot$meas_diam_min_cm <- 10
  plot$meas_pom_default_m <- 1.3
  plot$meas_tree_stem <- TRUE
  plot$meas_tree_group <- TRUE
  plot$meas_dead <- TRUE
  plot$meas_fallen <- FALSE
  plot$meas_plot_loc <- "GNSS"
  plot$meas_stem_loc <- "Tape"

  stem <- suppressMessages(emptyTable(stem_cols, 6))
  stem$record_id <- 1:6
  stem$site_id <- "Site1"
  stem$acquisition_id <- "2024-08-03"
  stem$plot_id <- rep(c("P1", "P2"), each = 3)
  stem$tree_id <- paste0("T", 1:6)
  stem$stem_id <- paste0("S", 1:6)
  stem$measurement_date <- decDate("2024-08-03")
  stem$measurement_date_res <- "YYYY-MM-DD"
  stem$taxon_name <- c("Burkea africana", "Burkea africana", "Brachystegia spiciformis",
    "Julbernardia paniculata", "Burkea africana", "Brachystegia spiciformis")
  stem$x_rel_m <- c(10, 50, 90, 20, 60, 80)
  stem$y_rel_m <- c(10, 50, 90, 30, 40, 70)
  stem$diam_cm <- c(12.1, 35.4, 20.0, 15.2, 50.3, 11.0)
  stem$pom_m <- 1.3
  stem$code <- "AS"

  pt <- suppressMessages(emptyTable(pt_cols, 8))
  pt$site_id <- "Site1"
  pt$plot_id <- rep(c("P1", "P2"), each = 4)
  pt$point_id <- rep(c("SW", "SE", "NE", "NW"), 2)
  pt$corner <- TRUE
  pt$x_rel_m <- rep(c(0, 100, 100, 0), 2)
  pt$y_rel_m <- rep(c(0, 0, 100, 100), 2)
  pt$rover_easting_utm_m <- c(500000, 500100, 500100, 500000) + rep(c(0, 500), each = 4)
  pt$rover_northing_utm_m <- rep(c(8000000, 8000000, 8000100, 8000100), 2)
  pt$crs_epsg <- 32733L

  taxon <- data.frame(
    site_id = "Site1",
    taxon_name = c("Burkea africana", "Brachystegia spiciformis", "Julbernardia paniculata"))

  height <- data.frame(
    site_id = "Site1",
    plot_id = "P1",
    taxon_name = "Burkea africana",
    diam_cm = c(12, 20, 35),
    height_m = c(6, 9, 12))

  wd <- data.frame(
    site_id = "Site1",
    taxon_name = "Burkea africana",
    wood_density_gcm3 = c(0.65, 0.70),
    wood_density_sd_gcm3 = NA_real_,
    wood_density_n = NA_integer_)

  list(plot = plot, stem = stem, pt = pt, taxon = taxon, height = height, wd = wd)
}

runCheck <- function(l) {
  valCheck(plot = l$plot, stem = l$stem, pt = l$pt, taxon = l$taxon,
    height = l$height, wd = l$wd, cols = cols)
}

test_that("valid tables pass without errors or warnings", {
  l <- makeL0()
  expect_silent(colCheck(l$stem, stem_cols))
  expect_silent(colCheck(l$plot, plot_cols))
  expect_silent(colCheck(l$pt, pt_cols))
  expect_silent(runCheck(l))
})

test_that("column spec tables are found where valCheck() is called", {
  l <- makeL0()
  l$stem$diam_cm[1] <- 2000
  expect_error(valCheck(stem = l$stem), "diam_cm` must be <= 1000")
})

test_that("stem table checks", {
  l <- makeL0(); l$stem$record_id[2] <- 1L
  expect_error(runCheck(l), "record_id")

  l <- makeL0(); l$stem$diam_cm[1] <- 0
  expect_error(runCheck(l), "`diam_cm` must be positive")

  l <- makeL0(); l$stem$pom_m[1] <- 60
  expect_error(runCheck(l), "pom_m` must be <= 50")

  l <- makeL0(); l$stem$measurement_date <- "2024-08-03"
  expect_error(runCheck(l), "decimal year")

  l <- makeL0(); l$stem$measurement_date[1] <- 24.5
  expect_error(runCheck(l), "between 1900")

  l <- makeL0(); l$stem$measurement_date_res[1] <- "DD-MM-YYYY"
  expect_error(runCheck(l), "measurement_date_res` must be one of")

  l <- makeL0(); l$stem$measurement_date_res[1] <- NA
  expect_error(runCheck(l), "measurement_date_res` must be provided")

  l <- makeL0(); l$stem$code[1] <- "AX"
  expect_error(runCheck(l), "`code` must only contain")

  l <- makeL0(); l$stem$code[1] <- "AH"
  expect_silent(runCheck(l))

  l <- makeL0(); l$stem$code[1] <- "AD"
  expect_error(runCheck(l), "'A' or 'D'")

  l <- makeL0(); l$stem$code[1] <- "ASBT"
  expect_error(runCheck(l), "'B' or 'T'")

  l <- makeL0(); l$stem$code[1] <- "ASM"
  expect_error(runCheck(l), "'M' \\(missing\\)")

  l <- makeL0(); l$stem$code[1] <- NA
  expect_warning(runCheck(l), "no `code`")

  l <- makeL0(); l$stem$code[1] <- "AST"
  expect_warning(runCheck(l), "'T' \\(broken below POM\\)")

  l <- makeL0(); l$stem$stem_id[1] <- NA
  expect_warning(runCheck(l), "no `stem_id`")
})

test_that("plot table checks", {
  l <- makeL0(); l$plot$plot_id[2] <- "P1"
  expect_error(runCheck(l), "`plot_id` must be unique")

  l <- makeL0(); l$plot$acquisition_id <- "03/08/2024"; l$stem$acquisition_id <- "03/08/2024"
  expect_error(runCheck(l), "YYYY, YYYY-MM, or YYYY-MM-DD")

  l <- makeL0(); l$plot$meas_dead[1] <- NA
  expect_error(runCheck(l), "NAs not allowed in: `meas_dead`")

  l <- makeL0(); l$plot$plot_planar[1] <- NA
  expect_error(runCheck(l), "NAs not allowed in: `plot_planar`")

  # Descriptions of methods should be provided
  l <- makeL0(); l$plot$meas_stem_loc[1] <- NA
  expect_warning(runCheck(l), "Please describe methods in: `meas_stem_loc`")

  # Lianas, palms and bamboos may be NA if not present
  l <- makeL0(); l$plot$meas_liana <- NA
  expect_silent(runCheck(l))

  l <- makeL0(); l$plot$meas_pom_default_m[1] <- 3
  expect_error(runCheck(l), "meas_pom_default_m` must be <= 2")
})

test_that("plot point table checks", {
  l <- makeL0(); l$pt$point_id[2] <- "SW"
  expect_error(runCheck(l), "`point_id` must be unique")

  l <- makeL0(); l$pt$crs_epsg[1] <- NA
  expect_error(runCheck(l), "crs_epsg")

  l <- makeL0(); l$pt$rover_northing_utm_m[1] <- NA
  expect_error(runCheck(l), "both be provided or both be NA")

  l <- makeL0(); l$pt$corner[1:2] <- FALSE
  expect_error(runCheck(l), "At least three corners needed in each plot: P1")

  # Missing coordinates for one corner are allowed
  l <- makeL0(); l$pt$rover_easting_utm_m[1] <- NA; l$pt$rover_northing_utm_m[1] <- NA
  expect_silent(runCheck(l))
})

test_that("height and wood density table checks", {
  l <- makeL0(); l$height$height_m[1] <- NA
  expect_error(runCheck(l), "`height_m` must be positive and not NA")

  l <- makeL0(); l$height$height_m[1] <- 150
  expect_error(runCheck(l), "height_m` must be <= 100")

  l <- makeL0(); l$wd$wood_density_gcm3[1] <- 1.6
  expect_warning(runCheck(l), "outside 0.08-1.39")

  l <- makeL0(); l$wd$wood_density_n <- c(2.5, 3)
  expect_error(runCheck(l), "positive integer")
})

test_that("checks across tables", {
  l <- makeL0(); l$wd$site_id <- "Site2"
  expect_error(runCheck(l), "`site_id` differs among tables")

  l <- makeL0(); l$stem$plot_id[1] <- "P3"
  expect_error(runCheck(l), "missing from 'plot'")

  l <- makeL0(); l$pt <- l$pt[l$pt$plot_id == "P1", ]
  expect_error(runCheck(l), "'plot_id' does not match between 'plot' and 'pt'")

  l <- makeL0(); l$stem$taxon_name[1] <- "Pterocarpus angolensis"
  expect_error(runCheck(l), "missing from 'taxon'")

  l <- makeL0(); l$stem$diam_cm[1] <- 5
  expect_warning(runCheck(l), "below `meas_diam_min_cm`")

  l <- makeL0(); l$stem$x_rel_m[1] <- 150
  expect_warning(runCheck(l), "outside the extent of their plot corners")

  # Stems just outside the plot boundary are allowed
  l <- makeL0(); l$stem$x_rel_m[1] <- 105
  expect_silent(runCheck(l))

  # Negative stem coordinates give a warning
  l <- makeL0(); l$stem$x_rel_m[1] <- -2
  expect_warning(runCheck(l), "x_rel_m` must be >= 0")
})
