# Tests for s3Download() in ./func.R, using a mock S3 client
# Run from the repository root: testthat::test_file("./tests/test_s3Download.R")

library(testthat)

# Repository root, whether run from the root or from ./tests/
root <- if (file.exists("func.R")) "." else ".."

source(file.path(root, "func.R"))

# Mock of a paws::s3() client, listing `keys` in pages of `page_size`
mockS3 <- function(keys, page_size = 1000) {
  downloaded <- character(0)
  list(
    list_objects_v2 = function(Bucket, Prefix, ContinuationToken = NULL) {
      k <- keys[startsWith(keys, Prefix)]
      start <- if (is.null(ContinuationToken)) 1 else as.integer(ContinuationToken)
      end <- min(start + page_size - 1, length(k))
      page <- if (length(k) == 0) character(0) else k[start:end]
      list(
        Contents = lapply(page, function(x) list(Key = x)),
        IsTruncated = end < length(k),
        NextContinuationToken = if (end < length(k)) as.character(end + 1) else NULL)
    },
    download_file = function(Bucket, Key, Filename) {
      if (!dir.exists(dirname(Filename))) {
        stop("Directory does not exist: ", dirname(Filename))
      }
      writeLines(Key, Filename)
      downloaded <<- c(downloaded, Key)
    },
    downloaded = function() downloaded
  )
}

test_that("all pages of files are downloaded, keeping sub-directories", {
  prefix <- "user/GEO-TREES_INV/dat/sites/Site1/L0"
  keys <- c(
    paste0(prefix, "/"),  # Directory placeholder
    sprintf("%s/stems_%04d.csv", prefix, 1:2499),
    paste0(prefix, "/shp/plot_corners.shp"),
    "user/GEO-TREES_INV/dat/sites/Site10/L0/other.csv")  # Different site
  client <- mockS3(keys)
  dest <- file.path(tempfile(), "L0")

  files <- suppressMessages(s3Download(client, "bucket", prefix, dest))

  expect_length(client$downloaded(), 2500)
  expect_true(all(file.exists(files)))
  expect_true(file.exists(file.path(dest, "shp", "plot_corners.shp")))
  expect_false(any(grepl("Site10", client$downloaded())))
})

test_that("prefix is matched literally, with or without a trailing slash", {
  prefix <- "user/dat/sites/Site.1+(a)/L0"
  keys <- paste0(prefix, c("/stems.csv", "/plots.csv"))

  for (p in c(prefix, paste0(prefix, "/"))) {
    client <- mockS3(keys)
    dest <- tempfile()
    files <- suppressMessages(s3Download(client, "bucket", p, dest))
    expect_setequal(basename(files), c("stems.csv", "plots.csv"))
    expect_true(all(dirname(files) == dest))
  }
})

test_that("an error is given if there are no files", {
  client <- mockS3(c("user/dat/sites/Site1/L0/"))
  expect_error(s3Download(client, "bucket", "user/dat/sites/Site1/L0", tempfile()),
    "No files found")
})
