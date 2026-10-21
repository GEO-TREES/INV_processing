# Tests for date functions in ./func.R
# Run from the repository root: testthat::test_file("./tests/test_date.R")

library(testthat)

# Repository root, whether run from the root or from ./tests/
root <- if (file.exists("func.R")) "." else ".."

source(file.path(root, "func.R"))

test_that("decToDate() reverses decDate() for full dates", {
  x <- c("2019-01-01", "2020-02-29", "2024-08-02", "2025-12-31")
  expect_equal(decToDate(decDate(x, digits = NULL)), as.Date(x))
})

test_that("decToDate() keeps missing values", {
  expect_equal(decToDate(c(NA, decDate("2024-08-02", digits = NULL))),
    as.Date(c(NA, "2024-08-02")))
})

test_that("decFormat() formats at the resolution of the date", {
  x <- decDate(c("2024-08-02", "2024-08", "2019"))
  expect_equal(decFormat(x, c("YYYY-MM-DD", "YYYY-MM", "YYYY")),
    c("2024-08-02", "2024-08", "2019"))
  expect_equal(decFormat(c(x[1], NA)), c("2024-08-02", NA))
})

test_that("readOpt() returns NULL for a missing file", {
  expect_null(readOpt(tempfile(fileext = ".csv")))
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:2), f, row.names = FALSE)
  expect_equal(readOpt(f)$a, 1:2)
})

test_that("decToDate() treats non-finite values as missing", {
  expect_true(all(is.na(decToDate(c(Inf, -Inf, NaN)))))
})
