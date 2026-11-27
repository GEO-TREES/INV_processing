# Functions to run the data processing for one or more sites
# John L. Godlee (johngodlee@gmail.com)
# Last updated: 2026-11-27

# In an R session, at the repository root. Sourcing this script only defines
# the functions:
#   source("./run.R")
#   runSite("./sites/Paracou/2019/v1/param.yaml", clean = TRUE)
#   runSiteAll(sites = c("Bicuar", "Amacayacu"))
#
# From the command line, see the end of this script. `--clean` deletes the 
# outputs of previous runs first:
#   Rscript run.R ./sites/Paracou/2019/v1/param.yaml --clean
#   Rscript run.R --sites=Bicuar,Amacayacu --clean
#   Rscript run.R --all --clean

# Load packages
library(dplyr)
library(tidyr)
library(readxl)
library(sf)
library(terra)
library(BIOMASS)
library(yaml)
library(rocrateR)
library(ggplot2)

#' Processing steps, in the order they are run
#'
#' The name of each step is the name of its script, and of its directory of
#' outputs. ./06_record_fil.R is run after ./01_fmt.R, because the steps 
#' which estimate wood density, height and AGB use its selection of stems.
#' ./01_fmt.R is in the acquisition directory, ./sites/<SITE>/<ACQUISITION>/
#'
step_order <- c(
  "01_fmt",
  "06_record_fil",
  "02_taxa",
  "03_wd",
  "04_height",
  "05_agb_stem",
  "07_quad",
  "08_agb_mc",
  "09_stem_summ",
  "10_quad_summ",
  "11_brm",
  "12_report"
)

#' Files written by the processing steps which other steps read
#'
#' @param e environment of the run, created by `runSite()`, with the column
#'     classes of the data tables and the output directories
#'
#' @return named list, one item per object read by a step. Each is a list
#'     with the `file` and, for .csv files, the column classes `class`. Files
#'     with `optional` TRUE are NULL in the step if they do not exist, e.g.
#'     wood density measurements, which not all sites have
#'
stepFiles <- function(e) {
  f <- function(dir, file, class = NA, optional = FALSE) {
    list(file = file.path(e$ver_dir, dir, file), class = class, optional = optional)
  }
  plot_id_class <- c(plot_id = "character")
  quad_id_class <- c(plot_id = "character", quadrat_id = "character")

  list(
    # Formatted data
    stem = f("01_fmt", "stem.csv", e$stem_col_class),
    plot = f("01_fmt", "plot.csv", e$plot_col_class),
    plot_pt = f("01_fmt", "plot_pt.csv", e$pt_col_class),
    height = f("01_fmt", "height.csv", e$height_col_class, optional = TRUE),
    wd = f("01_fmt", "wd.csv", e$wd_col_class, optional = TRUE),
    taxon = f("01_fmt", "taxon.csv", e$taxon_col_class, optional = TRUE),
    subplot = f("01_fmt", "subplot.csv", e$subplot_col_class, optional = TRUE),
    stem_dup = f("01_fmt", "stem_dup.csv", e$stem_dup_col_class),

    # Stem selection
    record_fil = f("06_record_fil", "record_fil.txt"),
    record_agbd = f("06_record_fil", "record_agbd.txt"),
    record_status = f("06_record_fil", "record_status.csv"),

    # Taxonomy, wood density, height, AGB
    taxa = f("02_taxa", "stem_taxa.csv"),
    taxa_outcome = f("02_taxa", "taxa_outcome.csv"),
    stem_wd = f("03_wd", "stem_wd.csv"),
    stem_height = f("04_height", "stem_height.csv"),
    plot_cent = f("04_height", "plot_cent.csv", plot_id_class),
    height_mod = f("04_height", "height_mod.rds"),
    height_mod_comp = f("04_height", "height_mod_comp.csv", optional = TRUE),
    stem_agb = f("05_agb_stem", "stem_agb.csv"),

    # Plots and quadrats
    quad_pt = f("07_quad", "quad_pt.gpkg"),
    quad_poly = f("07_quad", "quad_poly.gpkg"),
    plot_poly = f("07_quad", "plot_poly.gpkg"),
    stem_pt = f("07_quad", "stem_pt.gpkg"),
    poly_check = f("07_quad", "plot_poly_check.csv", plot_id_class),
    subplot_check = f("07_quad", "subplot_check.csv", 
      c(plot_id = "character", subplot_id = "character"), optional = TRUE),

    # AGB error propagation
    stem_agb_mc = f("08_agb_mc", "stem_agb_mc.csv"),
    quad_agb = f("08_agb_mc", "quad_agb.csv", quad_id_class),
    quad_agb_simu = f("08_agb_mc", "quad_agb_mc.csv", quad_id_class),
    plot_agb = f("08_agb_mc", "plot_agb.csv", plot_id_class),
    diam_error_fit = f("08_agb_mc", "diam_error.csv", optional = TRUE),

    # Summaries
    stem_summ = f("09_stem_summ", "stem_summ.gpkg"),
    quad_summ = f("10_quad_summ", "quad_summ.gpkg"),

    # Files to check results
    diam_dup = f("report", "diam_dup.csv", optional = TRUE),
    stem_outside = f("report", "stem_outside.csv", plot_id_class, optional = TRUE),
    tree_unmapped = f("report", "tree_unmapped.csv", plot_id_class, optional = TRUE)
  )
}

#' Objects read by each processing step
#'
#' @param param list of parameters, from ./00_param.R
#'
#' @return named list, one item per step in `step_order`, with the names of 
#'     the objects in `stepFiles()` which the step reads. Some objects are 
#'     only read with some methods, e.g. the coordinates of plot centres, 
#'     used by the regional height model
#'
stepInputs <- function(param) {
  h_regional <- if (param$height_method == "regional") "plot_cent"
  h_model <- if (param$height_method %in% c("field", "custom")) "height_mod"

  list(
    "01_fmt" = character(0),
    "06_record_fil" = c("stem", "plot"),
    "02_taxa" = c("stem", "wd", "height", "taxon"),
    "03_wd" = c("stem", "taxa", "wd", "plot", "record_agbd"),
    "04_height" = c("stem", "plot", "plot_pt", "height", "stem_wd", "record_agbd"),
    "05_agb_stem" = c("stem", "stem_wd", "stem_height", "plot", "record_agbd", 
      h_regional, h_model),
    "07_quad" = c("stem", "plot_pt", "record_agbd", "subplot"),
    "08_agb_mc" = c("stem", "stem_wd", "stem_height", "stem_agb", "record_fil", 
      "record_agbd", "quad_pt", "stem_pt", "plot", "plot_pt", "subplot", 
      h_regional, h_model, if (identical(param$diam_error, "field")) "stem_dup"),
    "09_stem_summ" = c("stem", "taxa", "stem_wd", "stem_height", "stem_agb", 
      "record_fil", "record_agbd", "stem_pt", "stem_agb_mc"),
    "10_quad_summ" = c("quad_poly", "quad_agb", "stem_summ"),
    "11_brm" = c("plot", "plot_pt", "plot_poly", "stem_summ", "quad_summ"),
    "12_report" = c("plot", "plot_pt", "plot_poly", "stem_summ", "quad_summ", 
      "quad_agb_simu", "plot_agb", "poly_check", "record_status", 
      "taxa_outcome", "height_mod_comp", "diam_error_fit", "diam_dup", 
      "stem_outside", "tree_unmapped", "subplot_check", "height", "wd", "taxon")
  )
}

#' Read a file written by a processing step
#'
#' @param x item of `stepFiles()`
#' @param name name of the object, used in error messages
#'
#' @return data frame from a .csv file, sf object from a .gpkg file, 
#'     character vector from a .txt file, or the object in an .rds file. NULL
#'     if the file is optional and does not exist
#'
stepRead <- function(x, name) {
  if (!file.exists(x$file)) {
    if (x$optional) {
      return(NULL)
    }
    stop("File of `", name, "` not found: ", x$file, 
      ". Run the step which writes it first", call. = FALSE)
  }
  switch(tolower(tools::file_ext(x$file)),
    csv = read.csv(x$file, colClasses = x$class),
    gpkg = st_read(x$file, quiet = TRUE),
    txt = readLines(x$file),
    rds = readRDS(x$file),
    stop("Don't know how to read ", x$file))
}

#' Delete the outputs of previous runs of a software version
#'
#' The WFO cache, which stores choices made during interactive taxonomic name 
#' matching, is kept.
#'
#' @param ver_dir directory of all outputs of a software version
#'
cleanOutputs <- function(ver_dir) {
  message("Deleting previous outputs...")
  files_rem <- list.files(ver_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  files_rem <- files_rem[!basename(files_rem) %in% c("wfo_cache.csv", "wfo_cache.rds")]
  invisible(file.remove(files_rem))

  # Delete empty sub-directories, deepest first
  dirs_rem <- setdiff(list.dirs(ver_dir, recursive = TRUE), ver_dir)
  for (i in dirs_rem[order(nchar(dirs_rem), decreasing = TRUE)]) {
    if (length(list.files(i, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(i, recursive = TRUE)
    }
  }
}

#' Load the WFO cache of taxonomic name matches of an acquisition
#'
#' The cache is shared by all product versions of an acquisition, and is
#' stored with the raw data, in the directory containing `raw_dir`. It is a 
#' .csv file, see `wfoCacheWrite()` in ./func.R, so that name matching choices 
#' can be reviewed without R. A cache saved as .rds by a previous version of 
#' the pipeline is converted, and the .rds file is not deleted.
#'
#' @param e environment of the run, created by `runSite()`
#'
wfoCacheLoad <- function(e) {
  wfo_path <- file.path(dirname(e$param$raw_dir), "wfo_cache.csv")
  if (!file.exists(wfo_path)) {
    wfo_old <- c(
      file.path(dirname(e$param$raw_dir), "wfo_cache.rds"),
      file.path(e$acq_dir, "wfo_cache.rds"), 
      file.path(e$ver_dir, "02_taxa", "wfo_cache.rds"))
    wfo_old <- wfo_old[file.exists(wfo_old)]
    if (length(wfo_old) > 0) {
      message("Converting ", wfo_old[1], " to ", wfo_path)
      e$wfoCacheWrite(wfo_path, cache = readRDS(wfo_old[1]))
    }
  }
  e$wfoCacheRead(wfo_path)
  wfo_path
}

#' Run the data processing for one product version of an acquisition
#'
#' Reads and checks the parameters, then runs the processing steps in 
#' `step_order`. Each step is a script, run in its own environment, with the 
#' parameters `param`, the functions in ./func.R, its output directory 
#' `outdir`, and the files it reads from other steps, see `stepInputs()`. 
#' Nothing is written to the global environment, and the function does not 
#' prompt, so it can be used in scripts and scheduled jobs.
#'
#' @param p_file path to the parameters file, 
#'     ./sites/<SITE>/<ACQUISITION>/<VERSION>/param.yaml
#' @param clean logical, if TRUE the outputs of previous runs of this software 
#'     version are deleted first, except the WFO cache
#' @param batch logical, if TRUE ambiguous taxonomic names are not prompted 
#'     for, see ./02_taxa.R. Sessions which are not interactive never prompt
#' @param from,to optional names of the first and last steps to run, see 
#'     `step_order`, e.g. `from = "08_agb_mc"` to repeat AGB error propagation 
#'     and the steps after it, using the outputs of earlier steps from a 
#'     previous run
#'
#' @return invisibly, a list with the parameters `param`, the directory of 
#'     outputs `ver_dir`, the directories of the L1, L2 and L3 datasets 
#'     `L_dir`, and the run time of each step in seconds `step_time`
#'
runSite <- function(p_file, clean = FALSE, batch = !interactive(), 
  from = NULL, to = NULL) {

  if (!file.exists("./func.R") || !file.exists("./00_param.R")) {
    stop("Run from the root of the INV_processing repository, not: ", getwd())
  }
  if (length(p_file) != 1 || !file.exists(p_file)) {
    stop("Parameters file not found: ", p_file)
  }

  # Steps to run
  from <- if (is.null(from)) step_order[1] else match.arg(from, step_order)
  to <- if (is.null(to)) step_order[length(step_order)] else match.arg(to, step_order)
  step_run <- step_order[seq_along(step_order) >= match(from, step_order) &
    seq_along(step_order) <= match(to, step_order)]
  if (length(step_run) == 0) {
    stop("`from` (", from, ") is after `to` (", to, "), steps are run in this order: ",
      paste(step_order, collapse = ", "))
  }
  if (clean && from != step_order[1]) {
    stop("`clean` deletes the outputs of all steps, so `from` cannot be set")
  }

  # Don't prompt for ambiguous taxonomic names in batch runs
  opt_old <- options(inv.batch = isTRUE(batch))
  on.exit(options(opt_old), add = TRUE)

  # Environment of the run, with functions, parameters, column classes of
  # data tables and output directories
  e <- new.env(parent = globalenv())
  source("./func.R", local = e)
  e$p_file <- p_file
  message("Parameters: ", p_file)
  source("./00_param.R", local = e)

  if (clean) {
    cleanOutputs(e$ver_dir)
  }

  # Create output directories
  for (i in c(step_order, e$L_list, "report")) {
    dir.create(file.path(e$ver_dir, i), showWarnings = FALSE, recursive = TRUE)
  }

  # If S3, copy raw data from S3 bucket to local directory
  if (e$param$opt_s3 && "01_fmt" %in% step_run) {
    # Define S3 bucket where MAAP user directories are located
    s3_client <- paws::s3(config = list(region = "us-west-2"))
    bucket <- "maap-ops-workspace"

    # Download all files for this version of the raw data
    e$s3Download(s3_client, bucket, 
      prefix = file.path(e$param$s3_dir, e$param$raw_version),
      dest = file.path(e$param$raw_dir, e$param$raw_version))
  }

  # Create BIOMASS cache
  e$BIOMASS_cache <- "./dat/BIOMASS/cache"
  BIOMASS::createCache(e$BIOMASS_cache)
  for (i in c("CWD.bil", "bio4.bil", "bio15.bil", "E.bil")) {
    BIOMASS::cacheManager(i)
  }

  # Raw data are versioned, in `raw_dir`/`raw_version`, so that outputs can be
  # reproduced after raw data are corrected
  e$indir <- file.path(e$param$raw_dir, e$param$raw_version)
  if ("01_fmt" %in% step_run && !dir.exists(e$indir)) {
    stop("Raw data directory not found: ", e$indir)
  }

  # Files read by steps. Each is read once, when first used by a step
  files <- stepFiles(e)
  inputs <- stepInputs(e$param)
  read_cache <- list()

  step_time <- setNames(rep(NA_real_, length(step_run)), step_run)
  for (s in step_run) {
    # Each step has its own environment, so that objects created by one step 
    # are not used by another without being written to file
    es <- new.env(parent = e)
    es$outdir <- if (s == "12_report") e$report_dir else file.path(e$ver_dir, s)

    for (i in inputs[[s]]) {
      if (!i %in% names(read_cache)) {
        # list(NULL) keeps optional files which do not exist in the cache
        read_cache[i] <- list(stepRead(files[[i]], i))
      }
      assign(i, read_cache[[i]], envir = es)
    }

    # AGB uses diameters corrected for taper, see `taper_method`
    if (s %in% c("05_agb_stem", "08_agb_mc")) {
      es$stem <- e$taperDiam(es$stem, es$stem_height)
    }
    if (s == "02_taxa") {
      es$wfo_path <- wfoCacheLoad(e)
    }

    script <- if (s == "01_fmt") {
      file.path(e$acq_dir, "01_fmt.R") 
    } else {
      paste0("./", s, ".R")
    }
    t0 <- Sys.time()
    e$runFn(script, es)
    step_time[s] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  }

  # List files written for site reports, and to check the results of each step
  message("Files for site reports and to check results, in ", e$report_dir, ":\n  ",
    paste(list.files(e$report_dir), collapse = "\n  "))
  message("Finished ", p_file, " in ", round(sum(step_time) / 60, 1), " minutes")

  invisible(list(
    param = e$param,
    ver_dir = e$ver_dir,
    L_dir = e$L_dir_list,
    step_time = step_time))
}

#' Run the data processing for several product versions, acquisitions or sites
#'
#' Runs `runSite()` for each parameters file. An error in one does not stop 
#' the others. 
#'
#' @param p_files paths to parameters files. By default, all files named
#'     param.yaml in ./sites/
#' @param sites optional names of sites, to run only the parameters files in
#'     ./sites/<SITE>/. The <SITE> directory is matched exactly, so e.g. 
#'     "Paracou" does not match "Paracou2"
#' @param clean,batch passed to `runSite()`. By default previous outputs are 
#'     deleted, and ambiguous taxonomic names are not prompted for
#'
#' @return data frame with one row per parameters file: `p_file`, `status`
#'     ("ok" or "error"), the error `message` if any, and the run time `mins`
#'
runSiteAll <- function(p_files = NULL, sites = NULL, clean = TRUE, batch = TRUE) {
  if (is.null(p_files)) {
    p_files <- list.files("./sites", "^param\\.yaml$", recursive = TRUE, full.names = TRUE)
  }
  if (!is.null(sites)) {
    p_files <- p_files[basename(dirname(dirname(dirname(p_files)))) %in% sites]
  }
  if (length(p_files) == 0) {
    stop("No parameters files found")
  }

  out <- lapply(p_files, function(i) {
    t0 <- Sys.time()
    msg <- tryCatch({
      runSite(i, clean = clean, batch = batch)
      NA_character_
    }, error = function(err) conditionMessage(err))
    data.frame(
      p_file = i, 
      status = if (is.na(msg)) "ok" else "error", 
      message = msg,
      mins = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1))
  })
  out <- do.call(rbind, out)

  message("Runs: ", sum(out$status == "ok"), " ok, ", sum(out$status == "error"), " with errors")
  if (any(out$status == "error")) {
    message(paste0("  ", out$p_file[out$status == "error"], ": ", 
      out$message[out$status == "error"], collapse = "\n"))
  }
  out
}

# Run from the command line: Rscript run.R ...
# Not run when this script is sourced, where `sys.nframe()` is above zero
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  usage <- paste0(
    "Usage, from the repository root:\n",
    "  Rscript run.R <param.yaml> [<param.yaml> ...] [--clean]\n",
    "  Rscript run.R --sites=<SITE>[,<SITE> ...] [--clean]\n",
    "  Rscript run.R --all [--clean]\n",
    "--clean deletes the outputs of previous runs first")

  opt <- args[startsWith(args, "--")]
  p_files <- args[!startsWith(args, "--")]
  sites <- sub("^--sites=", "", opt[startsWith(opt, "--sites=")])
  opt_bad <- setdiff(opt, c("--clean", "--all", opt[startsWith(opt, "--sites=")]))
  # One of: parameters files, --sites, --all
  n_mode <- (length(p_files) > 0) + (length(sites) > 0) + ("--all" %in% opt)
  if (length(opt_bad) > 0 || n_mode != 1 || length(sites) > 1) {
    message(usage)
    quit(status = 2)
  }
  clean <- "--clean" %in% opt

  if (length(p_files) == 1) {
    # One run. An error stops R with a non-zero exit status
    runSite(p_files, clean = clean)
  } else {
    # Several runs. The exit status is 1 if any failed
    runs <- runSiteAll(
      p_files = if (length(p_files) > 0) p_files,
      sites = if (length(sites) > 0) strsplit(sites, ",")[[1]],
      clean = clean)
    print(runs[, c("p_file", "status", "mins")])
    if (any(runs$status == "error")) {
      quit(status = 1)
    }
  }
}
