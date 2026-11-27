# Tests for running the pipeline from end to end, see `runSite()` in ./run.R
# Run from the repository root: testthat::test_file("./tests/test_runSite.R")

# The pipeline is run with the example data of the worked example,
# ./vignettes/worked_example/data-raw/data/, one 25 ha plot with 5000 stems,
# and the format script ./tests/fixture/sites/Site1/2024/01_fmt.R. Outputs
# are written to a temporary directory. The run takes about one minute

library(testthat)

# Repository root, whether run from the root or from ./tests/
root <- normalizePath(if (file.exists("func.R")) "." else "..")

# The pipeline uses paths relative to the repository root
old_wd <- setwd(root)
suppressPackageStartupMessages(source("./run.R"))

test_that("steps read files which are defined", {
  e <- new.env()
  e$ver_dir <- "out"
  files <- stepFiles(e)
  param <- list(height_method = "regional", diam_error = "field")
  inputs <- stepInputs(param)

  expect_setequal(names(inputs), step_order)
  expect_true(all(unlist(inputs) %in% names(files)))
  expect_true(all(file.exists(paste0("./", setdiff(step_order, "01_fmt"), ".R"))))

  # Each file is written by a step which runs before the steps which read it
  for (s in step_order) {
    dir_in <- basename(dirname(vapply(files[inputs[[s]]], `[[`, character(1), "file")))
    dir_in <- setdiff(dir_in, "report")
    expect_true(all(match(dir_in, step_order) < match(s, step_order)), info = s)
  }

  # Objects read only with some methods
  expect_true("plot_cent" %in% inputs[["08_agb_mc"]])
  expect_false("height_mod" %in% inputs[["08_agb_mc"]])
  expect_true("height_mod" %in% 
    stepInputs(list(height_method = "field", diam_error = "none"))[["08_agb_mc"]])
})

test_that("runSite() checks its arguments", {
  expect_error(runSite("./no/such/param.yaml"), "not found")
  p <- tempfile(fileext = ".yaml")
  writeLines("site_id: Site1", p)
  expect_error(runSite(p, from = "nonsense"), "should be one of")
  expect_error(runSite(p, from = "08_agb_mc", to = "02_taxa"), "is after")
  expect_error(runSite(p, clean = TRUE, from = "08_agb_mc"), "clean")
  expect_error(runSite(p), "Missing parameters")
})

test_that("runSiteAll() carries on after an error", {
  p <- tempfile(fileext = ".yaml")
  writeLines("site_id: Site1", p)
  runs <- suppressMessages(runSiteAll(p_files = c(p, p)))
  expect_equal(nrow(runs), 2)
  expect_equal(runs$status, c("error", "error"))
  expect_match(runs$message[1], "Missing parameters")
  expect_error(runSiteAll(sites = "NoSuchSite"), "No parameters files")
})

# Set up the example site in a temporary directory: raw data, WFO cache,
# format script and parameters file
fixtureSite <- function(dir) {
  data_dir <- "./vignettes/worked_example/data-raw/data"
  acq_dir <- file.path(dir, "sites", "Site1", "2024")
  dir.create(file.path(dir, "L0", "v1"), recursive = TRUE)
  dir.create(file.path(acq_dir, "v1"), recursive = TRUE)
  file.copy(
    file.path(data_dir, paste0(c("stem", "plot", "pt", "hd", "wd", "subplot"), ".csv")),
    file.path(dir, "L0", "v1"))
  file.copy(file.path(data_dir, "wfo_cache.csv"), dir)
  file.copy("./tests/fixture/sites/Site1/2024/01_fmt.R", acq_dir)

  param <- yaml::read_yaml("./tests/fixture/sites/Site1/2024/v1/param.yaml")
  param$out_dir <- file.path(dir, "out")
  param$raw_dir <- file.path(dir, "L0")
  p_file <- file.path(acq_dir, "v1", "param.yaml")
  yaml::write_yaml(param, p_file)
  p_file
}

test_that("pipeline runs from end to end on the example site", {
  skip_if_not(file.exists("./dat/BIOMASS/cache/E.bil"), 
    "BIOMASS cache not found, would be downloaded")
  skip_if(nzchar(Sys.getenv("INV_TEST_SKIP_RUN")), "INV_TEST_SKIP_RUN is set")

  dir <- tempfile("inv_run_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  p_file <- fixtureSite(dir)

  global_before <- ls(globalenv())
  run <- suppressWarnings(suppressMessages(runSite(p_file, clean = TRUE)))

  # Nothing is written to the global environment, and options are restored
  expect_setequal(ls(globalenv()), global_before)
  expect_null(getOption("inv.batch"))

  # Return value
  expect_equal(run$param$site_id, "Site1")
  expect_setequal(names(run$step_time), step_order)
  expect_true(all(run$step_time >= 0))

  # L1, L2, L3 datasets, each with its parameters and metadata
  for (i in c("L1", "L2", "L3")) {
    expect_true(all(c("param.yaml", "ro-crate-metadata.json") %in% 
      list.files(run$L_dir[[i]])), info = i)
  }
  expect_length(list.files(run$L_dir$L1, "_(stem|plot|pt)\\.csv$"), 3)
  expect_length(list.files(run$L_dir$L2, "_stem\\.csv$"), 1)
  l3_file <- list.files(run$L_dir$L3, "_quad\\.gpkg$", full.names = TRUE)
  expect_length(l3_file, 1)

  # L3: 100 quadrats of 50 x 50 m in a 500 x 500 m plot
  l3 <- sf::st_read(l3_file, quiet = TRUE)
  expect_equal(nrow(l3), 100)
  expect_equal(sum(l3$Quadrat_area), 25, tolerance = 0.01)
  agbd_col <- c("AGBD_stand_estimate", "AGBD_stand_median", 
    "AGBD_stand_uncertainty", "AGBD_stand_ci_lower", "AGBD_stand_ci_upper")
  expect_false(anyNA(sf::st_drop_geometry(l3)[, agbd_col]))
  expect_true(all(l3$AGBD_stand_ci_lower <= l3$AGBD_stand_median &
    l3$AGBD_stand_median <= l3$AGBD_stand_ci_upper))

  # Mean AGB density of quadrats, Mg ha-1, from a run on 2026-11-27. The 
  # random seed is fixed, so the tolerance only allows for differences
  # among versions of packages
  expect_equal(mean(l3$AGBD_stand_estimate), 92.37, tolerance = 0.01)

  # Stem selection is made once, by ./06_record_fil.R, and all stems used to
  # estimate AGB density have an AGB estimate
  record_agbd <- readLines(file.path(run$ver_dir, "06_record_fil", "record_agbd.txt"))
  record_fil <- readLines(file.path(run$ver_dir, "06_record_fil", "record_fil.txt"))
  expect_true(all(record_agbd %in% record_fil))
  stem_agb <- read.csv(file.path(run$ver_dir, "05_agb_stem", "stem_agb.csv"))
  expect_false(anyNA(stem_agb$agb_Mg[stem_agb$record_id %in% record_agbd]))

  # Later steps can be repeated from the outputs of earlier steps
  run2 <- suppressWarnings(suppressMessages(runSite(p_file, from = "09_stem_summ")))
  expect_equal(names(run2$step_time), 
    c("09_stem_summ", "10_quad_summ", "11_brm", "12_report"))
  l3_2 <- sf::st_read(l3_file, quiet = TRUE)
  expect_equal(l3_2$AGBD_stand_estimate, l3$AGBD_stand_estimate)

  # A step fails with a message if the files it reads are missing
  unlink(file.path(run$ver_dir, "08_agb_mc", "quad_agb.csv"))
  expect_error(suppressWarnings(suppressMessages(
    runSite(p_file, from = "10_quad_summ"))), "quad_agb")
})

setwd(old_wd)
