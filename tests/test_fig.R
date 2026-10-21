# Tests for functions used to create figures and console summaries in ./func.R
# Run from the repository root: testthat::test_file("./tests/test_fig.R")

library(testthat)

# Repository root, whether run from the root or from ./tests/
root <- if (file.exists("func.R")) "." else ".."

source(file.path(root, "func.R"))

test_that("plotFactor() orders numeric plot IDs numerically", {
  expect_equal(levels(plotFactor(c("10", "2", "1", "2"))), c("1", "2", "10"))
  expect_equal(as.character(plotFactor(c("10", "2"))), c("10", "2"))
})

test_that("plotFactor() orders other plot IDs alphabetically", {
  expect_equal(levels(plotFactor(c("b", "a", "10"))), c("10", "a", "b"))
})

test_that("figCols() gives a near-square layout", {
  expect_equal(figCols(1), 1L)
  expect_equal(figCols(4), 2L)
  expect_equal(figCols(12), 4L)
  expect_equal(figCols(0), 1L)
})

test_that("localXY() shifts each plot to its own origin", {
  x <- data.frame(plot_id = c("a", "a", "b", "b"), X = c(100, 150, 500, 520),
    Y = c(10, 30, 1000, 1010))
  out <- localXY(x, "X", "Y", "plot_id")
  expect_equal(out$X, c(0, 50, 0, 20))
  expect_equal(out$Y, c(0, 20, 0, 10))
})

test_that("localXY() uses a supplied origin", {
  x <- data.frame(plot_id = c("a", "b"), X = c(100, 500), Y = c(10, 1000))
  origin <- data.frame(plot_id = c("b", "a"), x0 = c(400, 90), y0 = c(900, 0))
  out <- localXY(x, "X", "Y", "plot_id", origin)
  expect_equal(out$X, c(10, 100))
  expect_equal(out$Y, c(10, 100))
})

test_that("squareExtent() returns a square centred on each plot", {
  x <- data.frame(plot_id = c("a", "a", "b", "b"), X = c(0, 100, 0, 20),
    Y = c(0, 50, 0, 40))
  out <- squareExtent(x, "X", "Y", "plot_id")
  expect_equal(names(out), c("plot_id", "X", "Y"))
  expect_equal(out$plot_id, c("a", "a", "b", "b"))
  expect_equal(out$X, c(0, 100, -10, 30))
  expect_equal(out$Y, c(-25, 75, 0, 40))
})

test_that("polyCoords() labels vertices with their plot", {
  sq <- function(x0) sf::st_polygon(list(rbind(c(x0, 0), c(x0 + 1, 0),
    c(x0 + 1, 1), c(x0, 1), c(x0, 0))))
  x <- sf::st_sf(plot_id = c("p1", "p2"), geometry = sf::st_sfc(sq(0), sq(5)))
  out <- polyCoords(x, "plot_id")
  expect_equal(nrow(out), 10)
  expect_equal(unique(out$plot_id), c("p1", "p2"))
  expect_equal(length(unique(out$group)), 2)
  expect_equal(range(out$X[out$plot_id == "p2"]), c(5, 6))
})

test_that("msgTable() prints a title and truncates long tables", {
  x <- data.frame(a = 1:5, b = letters[1:5])
  expect_message(msgTable(x, "My table:"), "My table:")
  expect_message(msgTable(x, max_rows = 2), "3 more rows")
  expect_null(suppressMessages(msgTable(x)))
})

test_that("wdLevelPlot() has one panel per level, in the order of the levels", {
  lev <- factor(c("genus", "species", "species"), levels = c("species", "genus", "family"))
  p <- wdLevelPlot(c(0.5, 0.6, 0.7), lev)
  b <- ggplot2::ggplot_build(p)
  expect_equal(as.character(b$layout$layout$level), c("species", "genus"))
})
