# Tests for crown allocation functions in ./func.R
# Run from the repository root: testthat::test_file("./tests/test_crown.R")

library(testthat)
suppressPackageStartupMessages(library(dplyr))

# Repository root, whether run from the root or from ./tests/
root <- if (file.exists("func.R")) "." else ".."

source(file.path(root, "func.R"))

# 3 x 2 grid of 20 m quadrats, starting 5 m from the origin corner
quad_corner <- bind_rows(lapply(seq_len(6), function(i) {
  ix <- (i - 1) %% 3
  iy <- (i - 1) %/% 3
  x0 <- 5 + 20 * ix
  y0 <- 5 + 20 * iy
  data.frame(quadrat_id = paste(ix, iy, sep = "_"),
    x_rel_m = c(x0, x0 + 20, x0 + 20, x0),
    y_rel_m = c(y0, y0, y0 + 20, y0 + 20))
}))
grid <- quadGrid(quad_corner)
tmpl <- crownTemplate()
homeQuad <- function(x, y) {
  match(paste(floor((x - 5) / 20), floor((y - 5) / 20), sep = "_"),
    grid$quad$quadrat_id)
}

crown_ok <- list(formula = "a * D^b", parameters = list(a = 0.57, b = 1.34),
  sigma = 0.549, link = "log", reference = "Test")

test_that("quadGrid() finds a regular grid of quadrats", {
  expect_equal(grid$nx, 3)
  expect_equal(grid$ny, 2)
  expect_equal(c(grid$x0, grid$y0, grid$qx, grid$qy), c(5, 5, 20, 20))
  expect_equal(grid$quad$quadrat_id, c("0_0", "1_0", "2_0", "0_1", "1_1", "2_1"))
})

test_that("quadGrid() returns NULL if the quadrats are not a regular grid", {
  expect_null(quadGrid(quad_corner[quad_corner$quadrat_id != "1_1", ]))
  irregular <- quad_corner
  irregular$x_rel_m[irregular$quadrat_id == "2_0"] <-
    irregular$x_rel_m[irregular$quadrat_id == "2_0"] + 3
  expect_null(quadGrid(irregular))
})

test_that("crownTemplate() weights sum to 1, with points inside the unit disc", {
  expect_equal(sum(tmpl$w), 1)
  expect_true(all(tmpl$u^2 + tmpl$v^2 < 1))
  expect_equal(length(tmpl$u), 16 * 32)
})

test_that("crownAlloc() assigns crowns inside the home quadrat to it", {
  out <- crownAlloc(15, 15, 10, 2, homeQuad(15, 15), grid, tmpl)
  expect_equal(out, c(10, 0, 0, 0, 0, 0))
})

test_that("crownAlloc() splits a crown centred on a quadrat edge equally", {
  out <- crownAlloc(25, 15, 10, 3, 1L, grid, tmpl)
  expect_equal(out, c(5, 5, 0, 0, 0, 0))
  out <- crownAlloc(25, 25, 10, 3, 1L, grid, tmpl)
  expect_equal(out, c(2.5, 2.5, 0, 2.5, 2.5, 0))
})

test_that("crownAlloc() matches the exact area of the crown in each quadrat", {
  # Share of a circle of radius 6 centred 3 m from a quadrat edge, in the
  # neighbouring quadrat: the area of a circular segment
  r <- 6
  d <- 3
  share <- (r^2 * acos(d / r) - d * sqrt(r^2 - d^2)) / (pi * r^2)
  out <- crownAlloc(22, 15, 1, r, homeQuad(22, 15), grid, tmpl)
  expect_equal(out[2], share, tolerance = 0.005)
  expect_equal(out[1], 1 - share, tolerance = 0.005)
})

test_that("crownAlloc() folds crowns outside the grid back into the grid", {
  out <- crownAlloc(6, 15, 10, 4, homeQuad(6, 15), grid, tmpl)
  expect_equal(sum(out), 10)
  expect_equal(out[1], 10)
})

test_that("crownAlloc() conserves the total AGB of the trees", {
  set.seed(1)
  n <- 300
  x <- runif(n, 5, 65)
  y <- runif(n, 5, 45)
  agb <- rlnorm(n)
  out <- crownAlloc(x, y, agb, runif(n, 0, 15), homeQuad(x, y), grid, tmpl)
  expect_equal(sum(out), sum(agb))
})

test_that("crownAlloc() keeps trees with a crown of radius 0 in their home quadrat", {
  x <- c(15, 35, 55)
  y <- c(15, 35, 15)
  out <- crownAlloc(x, y, c(1, 2, 3), c(0, 0, 0), homeQuad(x, y), grid, tmpl)
  expect_equal(out, c(1, 0, 3, 0, 2, 0))
})

test_that("crownCustom() builds a crown area allometry", {
  mod <- crownCustom(crown_ok)
  expect_equal(crownArea(mod, 50), 0.57 * 50^1.34)
  expect_equal(crownArea(mod, 50, z = 1), 0.57 * 50^1.34 * exp(0.549))
  mod_id <- crownCustom(modifyList(crown_ok, list(link = "identity", sigma = 20)))
  expect_equal(crownArea(mod_id, 50, z = 1), 0.57 * 50^1.34 + 20)
  # Crown area is at least 0
  expect_equal(crownArea(mod_id, 10, z = -100), 0)
})

test_that("crownCustom() rejects invalid allometries", {
  expect_error(crownCustom(crown_ok[c("formula", "parameters")]), "sigma")
  expect_error(crownCustom(modifyList(crown_ok, list(sigma = -1))), "sigma")
  expect_error(crownCustom(modifyList(crown_ok, list(link = "logit"))), "link")
  expect_error(crownCustom(modifyList(crown_ok,
    list(parameters = list(a = "x", b = 1.34)))), "named numbers")
  expect_error(crownCustom(modifyList(crown_ok, list(formula = "a * D^c"))),
    "not `D` or in `parameters`: c")
  expect_error(crownCustom(modifyList(crown_ok, list(formula = "-a * D^b"))),
    "positive crown area")
})
