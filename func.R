#' Run scripts from their own directory
#'
#' @param x filepath to R script 
#'
runFn <- function(x) {
  message(basename(x))
  stopifnot(file.exists(x))
  tryCatch(
    {
      source(x)
    },
    error = function(e) { 
      message("Error while running '", x, "': ", e$message)
      stop(e)
    }
  )
}

#' Check columns of a data table against a columns spec table
#'
#' @param x dataframe
#' @param cols column lookup table with three at least two columns:
#'     `column_name`, `class`
#'
colCheck <- function(x, cols) {
  
  # Check if extra columns in data table not in spec table
  cols_extra <- colnames(x)[!colnames(x) %in% cols$column_name]

  if (length(cols_extra) > 0) {
    stop("Extra columns found: ", paste(cols_extra, collapse = ", "))
  }

  # Check if missing columns in data table that are in spec table
  cols_missing <- cols$column_name[!cols$column_name %in% colnames(x)]

  if (length(cols_missing) > 0) {
    stop("Missing columns: ", paste(cols_missing, collapse = ", "))
  }

  # Check column classes
  col_class_test <- unlist(lapply(seq_len(nrow(cols)), function(y) { 
    any(class(x[[cols$column_name[y]]]) == cols$class[y])
  }))

  if (any(!col_class_test)) { 
    bad_cols <- cols$column_name[!col_class_test]
    bad_cols_good_class <- cols$class[match(bad_cols, cols$column_name)]
    bad_cols_bad_class <- unlist(lapply(bad_cols, function(y) { class(x[[y]]) }))

    stop("Columns with incorrect class: \n", 
      paste(
        "  ", 
        paste0(
          bad_cols, " should be `", bad_cols_good_class, 
          "` not `", bad_cols_bad_class, "`"), 
        collapse = "\n"
      )
    )
  }

  # Check column order is correct
  if (!identical(colnames(x), cols$column_name)) {
    stop("Column order incorrect", "\n", 
      "Correct order:", "\n",
      paste0("  ", cols$column_name, "\n"))
  }
}

#' Check values against minimum and maximum values in a column spec table
#'
#' @param x dataframe
#' @param cols column spec table with columns `column_name`, and optionally
#'     `min_val` and `max_val`
#' @param name name of the table, used in error messages
#' @param warn_cols columns for which values outside the range give a warning
#'     rather than an error
#'
rangeCheck <- function(x, cols, name = "x", warn_cols = NULL) {
  if (is.null(cols) || !all(c("min_val", "max_val") %in% names(cols))) {
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(cols))) {
    col <- cols$column_name[i]
    v <- suppressWarnings(as.numeric(x[[col]]))
    if (is.null(x[[col]]) || all(is.na(v))) {
      next
    }

    lo <- suppressWarnings(as.numeric(cols$min_val[i]))
    hi <- suppressWarnings(as.numeric(cols$max_val[i]))

    fail <- if (col %in% warn_cols) warning else stop

    if (!is.na(lo) && any(v < lo, na.rm = TRUE)) {
      fail("`", name, "$", col, "` must be >= ", lo, 
        " (", sum(v < lo, na.rm = TRUE), " values below)", call. = FALSE)
    }
    if (!is.na(hi) && any(v > hi, na.rm = TRUE)) {
      fail("`", name, "$", col, "` must be <= ", hi, 
        " (", sum(v > hi, na.rm = TRUE), " values above)", call. = FALSE)
    }
  }
}

#' Check stem table values
#' 
#' Runs various checks on the values in stem measurement table columns 
#'
#' @param x dataframe containing stem measurements
#' @param cols optional column spec table, e.g. ./templates/stem_cols.csv, 
#'     used to check minimum and maximum values
#'
stemValCheck <- function(x, cols = NULL) {
  # Record IDs must be unique
  if (any(duplicated(x$record_id))) {
    stop("Values of `record_id` must be unique within a site")
  }

  # Only one site ID per site
  if (length(unique(x$site_id)) > 1) { 
    stop("`site_id` must be the same for all plots within a site")
  }

  # All plots must have a name
  if (any(is.na(x$plot_id))) { 
    stop("NAs in `plot_id` are not allowed")
  }

  # All rows must have an acquisition ID 
  if (any(is.na(x$acquisition_id))) { 
    stop("NAs in `acquisition_id` are not allowed")
  }

  # Diameter measurements must be positive
  if (any(x$diam_cm <= 0, na.rm = TRUE)) { 
    stop("`diam_cm` must be positive")
  }

  # POM must be positive
  if (any(x$pom_m <= 0, na.rm = TRUE)) { 
    stop("`pom_m` must be positive")
  }

  # Height measurements must be positive
  if (any(x$height_m <= 0, na.rm = TRUE)) { 
    stop("`height_m` must be positive")
  }

  # Values within ranges defined in spec table
  # Stems may be mapped slightly outside the plot
  rangeCheck(x, cols, "stem", warn_cols = c("x_rel_m", "y_rel_m"))

  # Measurement dates must be decimal years, with a resolution
  if (!is.numeric(x$measurement_date)) {
    stop("`measurement_date` must be a decimal year, see `decDate()`")
  }

  if (any(x$measurement_date < 1900 | 
    x$measurement_date > as.numeric(format(Sys.Date(), "%Y")) + 1, na.rm = TRUE)) {
    stop("`measurement_date` must be a decimal year between 1900 and next year")
  }

  date_res <- c("YYYY", "YYYY-MM", "YYYY-MM-DD")
  if (any(!is.na(x$measurement_date_res) & !x$measurement_date_res %in% date_res)) {
    stop("`measurement_date_res` must be one of: ", paste(date_res, collapse = ", "))
  }

  if (any(!is.na(x$measurement_date) & is.na(x$measurement_date_res))) {
    stop("`measurement_date_res` must be provided where `measurement_date` is provided")
  }

  # Codes must only contain some values
  # See ./templates/stem_codes.csv
  code_allowed <- c("A", "D", "S", "F", "M", "B", "T", "U", "I", "H")
  if (any(grepl(paste0("[^", paste(code_allowed, collapse = ""), "]"), x$code))) {
    stop("`code` must only contain: ", paste(code_allowed, collapse = ", "))
  }

  # Some combinations of code are incompatible
  if (any(grepl("A", x$code) & grepl("D", x$code))){
    stop("`code` contain only either: 'A' or 'D', not both")
  }

  if (any(grepl("S", x$code) & grepl("F", x$code))){
    stop("`code` contain only either: 'S' or 'F', not both")
  }

  if (any(grepl("B", x$code) & grepl("T", x$code))){
    stop("`code` contain only either: 'B' or 'T', not both")
  }

  # If code is "M" (missing), diameter must be empty
  if (any(grepl("M", x$code) & (!is.na(x$diam_cm) | !is.na(x$height_m) | !is.na(x$pom_m)))) {
    stop("If `code` is 'M' (missing), diameter, height, and POM must be empty")
  }

  # Stems without a code are excluded from AGB estimates
  if (any(is.na(x$code) | x$code == "")) {
    warning(sum(is.na(x$code) | x$code == ""), " stems have no `code`, ", 
      "these will be excluded from AGB estimates")
  }

  # Diameter should not be measured on stems broken below the POM
  if (any(grepl("T", x$code) & !is.na(x$diam_cm))) {
    warning(sum(grepl("T", x$code) & !is.na(x$diam_cm)), 
      " stems with `code` 'T' (broken below POM) have a diameter measurement")
  }

  # Stems without a stem ID cannot be matched across measurements
  if (any(is.na(x$stem_id) | x$stem_id == "")) {
    warning(sum(is.na(x$stem_id) | x$stem_id == ""), " stems have no `stem_id`")
  }
}

#' Check taxon table values
#'
#' Runs various checks on the values in taxon table columns 
#'
#' @param x dataframe containing taxonomic data 
#' 
taxonValCheck <- function(x) { 
  # All taxonomic names must be filled
  if (any(is.na(x$taxon_name))) { 
    stop("NAs in `taxon_name` are not allowed")
  }

  # One row per taxonomic name
  if (any(duplicated(x$taxon_name))) {
    stop("`taxon_name` must be unique")
  }
}

#' Check height table values
#'
#' Runs various checks on the values in height table columns 
#'
#' @param x dataframe containing diameter and height data 
#' @param cols optional column spec table, e.g. ./templates/height_cols.csv, 
#'     used to check minimum and maximum values
#' 
heightValCheck <- function(x, cols = NULL) { 
  # Only one site ID per site
  if (length(unique(x$site_id)) > 1) { 
    stop("`site_id` must be the same for all plots within a site")
  }

  # Height must be positive and not NA
  if (any(x$height_m <= 0, na.rm = TRUE) | any(is.na(x$height_m))) { 
    stop("`height_m` must be positive and not NA")
  }

  # Diameter must be positive and not NA
  if (any(x$diam_cm <= 0, na.rm = TRUE) | any(is.na(x$diam_cm))) { 
    stop("`diam_cm` must be positive and not NA")
  }

  # Values within ranges defined in spec table
  rangeCheck(x, cols, "height")
}

#' Check wood density table values
#'
#' Runs various checks on the values in wood density table columns 
#'
#' @param x dataframe containing wood density data 
#' 
wdValCheck <- function(x) { 
  # Only one site ID per site
  if (length(unique(x$site_id)) > 1) { 
    stop("`site_id` must be the same for all plots within a site")
  }

  # Wood density must be positive and not NA
  if (any(x$wood_density_gcm3 <= 0, na.rm = TRUE) | any(is.na(x$wood_density_gcm3))) { 
    stop("`wood_density_gcm3` must be positive and not NA")
  }

  # Wood density should be within the range of the global wood density
  # database, as used by BIOMASS::AGBmonteCarlo()
  if (any(x$wood_density_gcm3 < 0.08 | x$wood_density_gcm3 > 1.39)) {
    warning(sum(x$wood_density_gcm3 < 0.08 | x$wood_density_gcm3 > 1.39), 
      " values of `wood_density_gcm3` are outside 0.08-1.39 g cm-3")
  }

  # Wood density SD must be positive
  if (any(x$wood_density_sd_gcm3 <= 0, na.rm = TRUE)) { 
    stop("`wood_density_sd_gcm3` must be positive")
  }

  # Wood density N must be positive integer
  if (any(x$wood_density_n <= 0 | x$wood_density_n != floor(x$wood_density_n), na.rm = TRUE)) { 
    stop("`wood_density_n` must be a positive integer")
  }
}

#' Check plot meta-data table values
#' 
#' Runs various checks on the values in plot meta-data table columns 
#'
#' @param x dataframe containing plot meta-data 
#' @param cols optional column spec table, e.g. ./templates/plot_cols.csv, 
#'     used to check minimum and maximum values, and that `vital` columns are 
#'     filled
#'
plotValCheck <- function(x, cols = NULL) {
  # Only one site ID per site
  if (length(unique(x$site_id)) > 1) { 
    stop("`site_id` must be the same for all plots within a site")
  }

  # Only one entry per plot
  if (any(table(x$plot_id) > 1)) { 
    stop("`plot_id` must be unique within the site")
  }

  # All rows must have a plot ID 
  if (any(is.na(x$plot_id))) { 
    stop("NAs in `plot_id` are not allowed")
  }

  # All rows must have an acquisition ID 
  if (any(is.na(x$acquisition_id))) { 
    stop("NAs in `acquisition_id` are not allowed")
  }

  # All acquisition IDs must be either YYYY, YYYY-MM, YYYY-MM-DD
  if (any(!grepl("^\\d{4}(-\\d{2}){0,2}$", x$acquisition_id))) {
    stop("`acquisition_id` must be formatted either YYYY, YYYY-MM, or YYYY-MM-DD")
  }

  # Plot length must be positive
  if (!all(!is.na(x$plot_length_m) & x$plot_length_m > 0)) {
    stop("`plot_length_m` must be positive")
  }

  # Plot width must be positive
  if (!all(!is.na(x$plot_width_m) & x$plot_width_m > 0)) {
    stop("`plot_width_m` must be positive")
  }

  # Minimum diameter threshold must be positive
  if (!all(!is.na(x$meas_diam_min_cm) & x$meas_diam_min_cm > 0)) {
    stop("`meas_diam_min_cm` must be positive")
  }

  # Default POM must be positive
  if (!all(!is.na(x$meas_pom_default_m) & x$meas_pom_default_m > 0)) {
    stop("`meas_pom_default_m` must be positive")
  }

  # Vital columns must be provided
  # Lianas, palms and bamboos may be NA if not present in the plot
  # Missing descriptions of methods (character columns) give a warning
  vital <- c("plot_planar", "meas_tree_stem", "meas_tree_group", "meas_dead", 
    "meas_fallen")
  vital_desc <- character(0)
  if (!is.null(cols) && "vital" %in% names(cols)) {
    vital_all <- cols$column_name[cols$vital %in% c(TRUE, "TRUE")]
    vital_all <- setdiff(vital_all, c("meas_liana", "meas_palm", "meas_bamboo"))
    vital_desc <- intersect(vital_all, cols$column_name[cols$class == "character"])
    vital <- setdiff(vital_all, vital_desc)
  }
  vital_na <- vital[vapply(vital, function(y) any(is.na(x[[y]])), logical(1))]
  if (length(vital_na) > 0) {
    stop("NAs not allowed in: ", paste(paste0("`", vital_na, "`"), collapse = ", "))
  }
  desc_na <- vital_desc[vapply(vital_desc, function(y) any(is.na(x[[y]])), logical(1))]
  if (length(desc_na) > 0) {
    warning("Please describe methods in: ", paste(paste0("`", desc_na, "`"), collapse = ", "))
  }

  # Values within ranges defined in spec table
  rangeCheck(x, cols, "plot")
}

#' Check plot geo-location sf object values
#' 
#' Runs various checks on the values in plot geo-location sf object columns 
#'
#' @param x sf dataframe containing plot geo-location points
#' @param cols optional column spec table, e.g. ./templates/pt_cols.csv, 
#'     used to check minimum and maximum values
#'
ptValCheck <- function(x, cols = NULL) {
  # Only one site ID per site
  if (length(unique(x$site_id)) > 1) { 
    stop("`site_id` must be the same for all plots within a site")
  }

  # All plots must have a name
  if (any(is.na(x$plot_id))) { 
    stop("NAs in `plot_id` are not allowed")
  }

  # All points must have a name
  if (any(is.na(x$point_id))) { 
    stop("NAs in `point_id` are not allowed")
  }

  # Point IDs must be unique within a plot
  if (any(unlist(lapply(split(x, x$plot_id), function(y) { 
        any(duplicated(y$point_id))
    })))) {
    stop("`point_id` must be unique within a plot")
  }

  # All X coordinates must be filled
  if (any(is.na(x$x_rel_m))) { 
    stop("NAs in `x_rel_m` are not allowed")
  }

  # All Y coordinates must be filled
  if (any(is.na(x$y_rel_m))) { 
    stop("NAs in `y_rel_m` are not allowed")
  }

  # Points must be identified as corners or not
  if (any(is.na(x$corner))) {
    stop("NAs in `corner` are not allowed")
  }

  # Coordinate reference system needed to locate points
  if (any(is.na(x$crs_epsg))) {
    stop("NAs in `crs_epsg` are not allowed")
  }

  # Easting and northing must both be provided, or both be missing
  if (any(is.na(x$rover_easting_utm_m) != is.na(x$rover_northing_utm_m))) {
    stop("`rover_easting_utm_m` and `rover_northing_utm_m` must both be provided or both be NA")
  }

  # At least three corners per plot to define a polygon
  n_corner <- tapply(paste(x$x_rel_m, x$y_rel_m)[x$corner %in% TRUE], 
    x$plot_id[x$corner %in% TRUE], function(y) length(unique(y)))
  n_corner <- n_corner[as.character(unique(x$plot_id))]
  n_corner[is.na(n_corner)] <- 0
  if (any(n_corner < 3)) {
    stop("At least three corners needed in each plot: ", 
      paste(names(n_corner)[n_corner < 3], collapse = ", "))
  }

  # Values within ranges defined in spec table
  rangeCheck(x, cols, "pt")
}

#' Match repeat diameter measurements to their original stem measurement
#'
#' Repeat measurements are matched to records in the stem table with the same
#' `plot_id` and `stem_id`. Where a stem has more than one record, e.g.
#' measured at more than one POM, the record with the closest POM, then the
#' closest measurement date, is used
#'
#' @param x repeat diameter measurements table, see 
#'     ./templates/stem_dup_cols.csv
#' @param stem stem measurement table
#'
#' @return integer vector, the row of `stem` matching each row of `x`, NA if
#'     the stem is not in `stem`
#'
dupMatch <- function(x, stem) {
  stem_ok <- !is.na(stem$stem_id) & stem$stem_id != ""
  stem_rows <- split(which(stem_ok), 
    paste(stem$plot_id, stem$stem_id, sep = "::")[stem_ok])
  key <- paste(x$plot_id, x$stem_id, sep = "::")

  vapply(seq_len(nrow(x)), function(j) {
    i <- stem_rows[[key[j]]]
    if (is.null(i) || is.na(x$stem_id[j])) {
      return(NA_integer_)
    }
    if (length(i) > 1) {
      pom_d <- abs(stem$pom_m[i] - x$pom_m[j])
      date_d <- abs(stem$measurement_date[i] - x$measurement_date[j])
      i <- i[order(ifelse(is.na(pom_d), Inf, pom_d), 
        ifelse(is.na(date_d), Inf, date_d))]
    }
    as.integer(i[1])
  }, integer(1))
}

#' Check repeat diameter measurement table values
#'
#' Runs various checks on the values in repeat diameter measurement table
#' columns, and against the stem measurement table if provided
#'
#' @param x dataframe containing repeat diameter measurements, see
#'     ./templates/stem_dup_cols.csv
#' @param stem optional stem measurement table
#' @param cols column spec table for repeat diameter measurements
#'
dupValCheck <- function(x, stem = NULL, cols = NULL) {
  # Only one site ID per site
  if (length(unique(x$site_id)) > 1) {
    stop("`site_id` must be the same for all plots within a site")
  }

  # Record IDs must be unique
  if (any(is.na(x$record_id)) || any(duplicated(x$record_id))) {
    stop("`record_id` must be unique and not NA in repeat diameter measurements")
  }

  # Stem ID is needed to match the original measurement
  if (any(is.na(x$stem_id) | x$stem_id == "")) {
    stop("`stem_id` must not be NA in repeat diameter measurements")
  }

  # Diameter must be positive and not NA
  if (any(x$diam_cm <= 0, na.rm = TRUE) | any(is.na(x$diam_cm))) {
    stop("`diam_cm` must be positive and not NA in repeat diameter measurements")
  }

  # Values within ranges defined in spec table
  rangeCheck(x, cols, "stem_dup")

  if (!is.null(stem)) {
    # Repeat measurements must refer to stems in the stem table
    i <- dupMatch(x, stem)
    if (any(is.na(i))) {
      stop(sum(is.na(i)), " repeat diameter measurements have a `plot_id` ",
        "and `stem_id` which are not in 'stem'")
    }

    # Original measurements without a diameter can't be compared
    if (any(is.na(stem$diam_cm[i]))) {
      warning(sum(is.na(stem$diam_cm[i])), " repeat diameter measurements ",
        "refer to stems in 'stem' without `diam_cm`, these will be ignored",
        call. = FALSE)
    }

    # Repeat measurements should be at the same POM
    pom_diff <- abs(x$pom_m - stem$pom_m[i]) > 0.05
    if (any(pom_diff, na.rm = TRUE)) {
      warning(sum(pom_diff, na.rm = TRUE), " repeat diameter measurements ",
        "have a different `pom_m` to the original measurement, these will be ",
        "ignored", call. = FALSE)
    }

    # Repeat measurements should be soon after the original measurement
    date_diff <- abs(x$measurement_date - stem$measurement_date[i]) > 1
    if (any(date_diff, na.rm = TRUE)) {
      warning(sum(date_diff, na.rm = TRUE), " repeat diameter measurements ",
        "are more than one year from the original measurement, these will be ",
        "ignored", call. = FALSE)
    }
  }
}

#' Check values across data objects
#' 
#' Runs various checks on values across plot, stem, pt and taxon objects
#'
#' @param plot plot meta-data table
#' @param stem stem measurement table
#' @param pt plot geo-location points table
#' @param taxon taxon table
#' @param height height-diameter measurements table
#' @param wd wood density table
#' @param dup repeat diameter measurements table
#' @param cols optional named list of column spec tables, with names
#'     `plot`, `stem`, `pt`, `height`, `dup`. If NULL, objects named
#'     `plot_cols`, `stem_cols`, `pt_cols`, `height_cols`, `stem_dup_cols` are
#'     used if they exist where `valCheck()` is called from
#'
valCheck <- function(plot = NULL, stem = NULL, pt = NULL, taxon = NULL, 
  height = NULL, wd = NULL, dup = NULL, cols = NULL) { 

  # Find column spec tables
  if (is.null(cols)) {
    env <- parent.frame()
    cols <- lapply(c(plot = "plot", stem = "stem", pt = "pt", height = "height",
      dup = "stem_dup"), 
      function(y) get0(paste0(y, "_cols"), envir = env))
  }

  # Run single table checks
  if (!is.null(plot)) {
    plotValCheck(plot, cols$plot)
  }

  if (!is.null(stem)) {
    stemValCheck(stem, cols$stem)
  }

  if (!is.null(pt)) {
    ptValCheck(pt, cols$pt)
  }

  if (!is.null(taxon)) {
    taxonValCheck(taxon)
  }

  if (!is.null(height)) {
    heightValCheck(height, cols$height)
  }

  if (!is.null(wd)) {
    wdValCheck(wd)
  }

  if (!is.null(dup)) {
    dupValCheck(dup, stem, cols$dup)
  }

  # All tables must have the same site ID
  site_ids <- unique(unlist(lapply(list(plot, stem, pt, taxon, height, wd, dup), 
    function(y) y$site_id)))
  if (length(site_ids) > 1) {
    stop("`site_id` differs among tables: ", paste(site_ids, collapse = ", "))
  }

  # Run tests to match plot and stem tables
  if (!is.null(plot) && !is.null(stem)) {
    # All plots and censuses in stem must be in plot
    if (!all(paste(stem$plot_id, stem$acquisition_id) %in% paste(plot$plot_id, plot$acquisition_id))) {
      stop("Some plot-acquisition combinations in 'stem' are missing from 'plot'")
    }

    # Acquisition IDs must match
    if (!setequal(plot$acquisition_id, stem$acquisition_id)) {
      stop("Acquisition IDs do not match between 'plot' and 'stem'")
    }

    # Stems with diameter below the minimum diameter threshold
    diam_min <- plot$meas_diam_min_cm[match(stem$plot_id, plot$plot_id)]
    if (any(stem$diam_cm < diam_min, na.rm = TRUE)) {
      warning(sum(stem$diam_cm < diam_min, na.rm = TRUE), 
        " stems have `diam_cm` below `meas_diam_min_cm`, ",
        "these will be excluded from AGB density estimates")
    }
  }

  # Run tests to match plot and pt tables
  if (!is.null(plot) && !is.null(pt)) {
    # All plots must match between plot and pt
    if (!setequal(pt$plot_id, plot$plot_id)) {
      stop("'plot_id' does not match between 'plot' and 'pt'")
    }
  }

  # Run tests to match stem and pt tables
  if (!is.null(stem) && !is.null(pt)) {
    # Stems should be within the extent of the plot corners, with 10% tolerance
    ext <- do.call(rbind, lapply(split(pt, pt$plot_id), function(y) {
      data.frame(
        plot_id = y$plot_id[1],
        x_min = min(y$x_rel_m), x_max = max(y$x_rel_m),
        y_min = min(y$y_rel_m), y_max = max(y$y_rel_m))
    }))
    e <- ext[match(stem$plot_id, ext$plot_id), ]
    tol_x <- (e$x_max - e$x_min) * 0.1
    tol_y <- (e$y_max - e$y_min) * 0.1
    out <- stem$x_rel_m < e$x_min - tol_x | stem$x_rel_m > e$x_max + tol_x | 
      stem$y_rel_m < e$y_min - tol_y | stem$y_rel_m > e$y_max + tol_y
    if (any(out, na.rm = TRUE)) {
      warning(sum(out, na.rm = TRUE), 
        " stems are more than 10% outside the extent of their plot corners")
    }
  }

  # Run tests to match stem and taxon tables
  if (!is.null(stem) && !is.null(taxon)) {
    # All taxonomic names in stem must be in taxon
    stem_taxa <- stem$taxon_name[!is.na(stem$taxon_name) & stem$taxon_name != ""]
    taxon_missing <- setdiff(stem_taxa, taxon$taxon_name)
    if (length(taxon_missing) > 0) {
      stop(length(taxon_missing), " values of `taxon_name` in 'stem' are missing from 'taxon'")
    }
  }
}

#' Download all files under a prefix in an AWS S3 bucket to a local directory
#'
#' Files are listed in pages of up to 1000, so all files are found. The
#' directory structure below `prefix` is recreated in `dest`. 
#'
#' @param client S3 client created by `paws::s3()`
#' @param bucket name of the S3 bucket
#' @param prefix S3 key prefix of the files to download, e.g. 
#'     `"johngodlee/GEO-TREES_INV/dat/sites/Bicuar/L0"`
#' @param dest local directory to download files to
#'
#' @return invisibly, a character vector of the local file paths
#'
s3Download <- function(client, bucket, prefix, dest) {
  # Treat the prefix as a directory
  prefix <- paste0(sub("/+$", "", prefix), "/")

  # List all objects, one page at a time
  keys <- character(0)
  token <- NULL
  repeat {
    resp <- if (is.null(token)) {
      client$list_objects_v2(Bucket = bucket, Prefix = prefix)
    } else {
      client$list_objects_v2(Bucket = bucket, Prefix = prefix, 
        ContinuationToken = token)
    }
    keys <- c(keys, vapply(resp$Contents, function(x) x$Key, character(1)))
    if (!isTRUE(resp$IsTruncated)) {
      break
    }
    token <- resp$NextContinuationToken
  }

  # Ignore directory placeholder objects
  keys <- keys[!grepl("/$", keys)]

  if (length(keys) == 0) { 
    stop("No files found in S3 bucket '", bucket, "' under: ", prefix)
  }

  # Local paths, keeping the structure below the prefix
  files <- file.path(dest, substring(keys, nchar(prefix) + 1))

  for (i in seq_along(keys)) {
    dir.create(dirname(files[i]), recursive = TRUE, showWarnings = FALSE)
    client$download_file(Bucket = bucket, Key = keys[i], Filename = files[i])
  }

  message("Downloaded ", length(keys), " files from S3 to: ", dest)
  invisible(files)
}

#' Select stem measurements used to estimate AGB
#'
#' Selects stems which are alive, standing, have a diameter measurement, and
#' are not broken below the POM or missing. Where a stem was measured more
#' than once, e.g. at different POMs, the measurement with the highest POM,
#' latest date, and largest diameter is used. Stems without coordinates, and
#' stems below the minimum diameter threshold of the plot, are included. See
#' `agbdFilter()` for stems used to estimate AGB density, and to form
#' multi-stemmed trees.
#'
#' @param stem stem measurement table, see ./templates/stem_cols.csv
#' @param plot plot meta-data table, see ./templates/plot_cols.csv
#'
#' @return character vector of `record_id` values
#'
recordFilter <- function(stem, plot) {
  stem %>%
    filter(
      !is.na(diam_cm),
      grepl("A", code),
      grepl("S", code),
      !grepl("T", code),
      !grepl("M", code)) %>%
    # Keep one measurement per stem. Stems without a `stem_id` cannot be
    # matched, so treat each of their records as a separate stem
    mutate(stem_key = ifelse(is.na(stem_id) | stem_id == "",
      paste0("record_", record_id), stem_id)) %>%
    group_by(site_id, plot_id, tree_id, stem_key) %>%
    slice_max(
      order_by = tibble(pom_m, measurement_date, diam_cm),
      n = 1,
      with_ties = FALSE) %>%
    ungroup() %>%
    pull(record_id) %>%
    as.character()
}

#' Select stem measurements used to estimate AGB density
#'
#' Selects stems from `recordFilter()` with a diameter at or above the minimum
#' diameter threshold, and optionally excludes stems broken above the POM.
#' Other stems have their own AGB estimates, but are not part of multi-stemmed
#' trees, and are not included in AGB density, so that AGB density matches the
#' sampling of the plot.
#'
#' @param stem stem measurement table, see ./templates/stem_cols.csv
#' @param plot plot meta-data table, see ./templates/plot_cols.csv
#' @param record_fil `record_id` values from `recordFilter()`
#' @param diam_min_cm minimum diameter threshold for AGB density. The larger
#'     of this and the minimum diameter threshold of the plot is used
#' @param include_broken logical, if FALSE stems broken above the POM ("B" in
#'     `code`) are excluded
#'
#' @return character vector of `record_id` values
#'
agbdFilter <- function(stem, plot, record_fil, diam_min_cm = 0, 
  include_broken = TRUE) {
  diam_min <- plot$meas_diam_min_cm[match(stem$plot_id, plot$plot_id)]
  diam_min <- pmax(diam_min, diam_min_cm)
  keep <- as.character(stem$record_id) %in% as.character(record_fil) &
    !is.na(stem$diam_cm) & !is.na(diam_min) & stem$diam_cm >= diam_min
  if (!include_broken) {
    keep <- keep & !grepl("B", stem$code)
  }
  as.character(stem$record_id[keep])
}

#' Calculate the equivalent diameter of multi-stemmed trees
#'
#' The equivalent diameter is the diameter of a single stem with the same 
#' basal area as all stems of the tree: \eqn{\sqrt{\sum D_i^2}}. AGB is 
#' estimated for the tree from the equivalent diameter, then shared among its
#' stems in proportion to their basal area.
#'
#' @param stem stem measurement table, with columns `record_id`, `plot_id`,
#'     `tree_id`, `diam_cm`
#' @param include `record_id` values of stems which form trees, e.g. from
#'     `recordFilter()`. Other stems, and stems without a `tree_id`, are 
#'     treated as single-stemmed trees
#'
#' @return dataframe with one row per row of `stem`, with columns:
#'     `record_id`, `tree_key` (identifies the tree within the site), 
#'     `diam_eq_cm` (equivalent diameter of the tree), `ba_share` (proportion
#'     of the basal area of the tree in this stem), and `n_stem_tree` (number
#'     of stems in the tree)
#'
treeEquiv <- function(stem, include) {
  inc <- as.character(stem$record_id) %in% as.character(include) &
    !is.na(stem$tree_id) & stem$tree_id != "" & !is.na(stem$diam_cm)
  tree_key <- ifelse(inc, paste(stem$plot_id, stem$tree_id, sep = "::"),
    paste0("record::", stem$record_id))

  ba <- stem$diam_cm^2
  ba_tree <- ave(ifelse(is.na(ba), 0, ba), tree_key, FUN = sum)

  out <- data.frame(
    record_id = stem$record_id,
    tree_key = tree_key,
    diam_eq_cm = ifelse(inc, sqrt(ba_tree), stem$diam_cm),
    ba_share = ifelse(inc, ba / ba_tree, 1),
    n_stem_tree = ave(rep(1L, nrow(stem)), tree_key, FUN = length))
  out
}

#' Paste values together, replace NAs with blank, optional separator
#'
#' @param ... vectors or dataframe to be pasted together
#' @param sep separator between adjacent values in vectors
#' @param collapse separator between sets of values across vectors
#' @param unique logical, if TRUE duplicated values are removed
#' @param sort logical, if TRUE values are sorted
#'
#' @return character vector 
#' 
#' @export
#' 
pasteVals <- function(..., sep = "", collapse = NULL, 
  remna = TRUE, unique = FALSE, sort = FALSE) {
  x <- as.matrix(cbind(...))

  if (remna && !unique && !sort) {
    # Join columns one at a time, skipping NAs. Much faster than apply() 
    # over rows for large tables
    ret <- x[, 1]
    for (i in seq_len(ncol(x))[-1]) {
      ret <- ifelse(is.na(ret), x[, i], 
        ifelse(is.na(x[, i]), ret, paste0(ret, sep, x[, i])))
    }
    ret <- unname(as.character(ret))
  } else {
    ret <-
      apply(
        X = x,
        MARGIN = 1,
        FUN = function(x) {
          if (all(is.na(x))) {
            NA_character_
          } else {
            if (remna) {
              x <- x[!is.na(x)]
            }
            if (unique) {
              x <- x[!duplicated(x)]
            }
            if (sort) {
              x <- sort(x, na.last = TRUE)
            }
            paste(x, collapse = sep)
          }
        }
      )
  }

  if (!is.null(collapse)) {
    paste(ret, collapse = collapse)
  } else {
    ret
  }
}

#' Get valid UTM zone from latitude and longitude in WGS84 decimal degrees
#'
#' @param lon vector of longitude coordinates in decimal degrees
#' @param lat vector of latitude coordinate in decimal degrees
#' @param epsg logical, if TRUE return EPSG code, otherwise UTM zone name 
#'
#' @return Vector of UTM zones for each latitude-longitude pair
#' 
#' @export
#'
utmZone <- function(lon, lat, epsg = TRUE) {
  # Calculate the zone number for all coordinates at once
  zone <- (floor((lon + 180) / 6) %% 60) + 1
  
  # Determine hemisphere (N or S)
  hemisphere <- ifelse(lat < 0, "S", "N")
  
  if (epsg) {
    # EPSG: 326xx for North, 327xx for South
    base_code <- ifelse(lat < 0, 32700, 32600)
    return(base_code + zone)
  } else {
    # Return string format like "18N"
    return(paste0(zone, hemisphere))
  }
}

#' Create empty columns in a dataframe based on a vector of column names
#'
#' @param x dataframe 
#' @param col_names vector of column names
#' @param col_classes vector of column classes corresponding to `col_names`.
#' 
#' @return Dataframe with original and new columns filled with NAs
#'
#' @export
#'
colGen <- function(x, col_names, col_classes) {
  
  # Ensure the inputs match up
  if (length(col_names) != length(col_classes)) {
    stop("`col_names` and `col_classes` must be the same length.")
  }
  
  # Find missing columns by index
  missing_idx <- which(!col_names %in% colnames(x))
  
  if (length(missing_idx) > 0) {
    new_cols <- col_names[missing_idx]
    new_classes <- col_classes[missing_idx]
    
    # Message
    message("New columns:\n  ", paste(new_cols, collapse = ",\n  "))
    
    # Add each new column with the correct NA type
    for (i in seq_along(new_cols)) {
      col_name <- new_cols[i]
      col_class <- new_classes[i]
      
      # Dynamically call the appropriate base R coercion function (e.g., as.numeric, as.character)
      coerce_fun <- match.fun(paste0("as.", col_class))
      
      # Populate the new column
      x[[col_name]] <- rep(coerce_fun(NA), nrow(x))
    }
  }
  
  return(x)
}

#' Extract function names and titles from roxygen2 markup from an R script
#'
#' Reads an R script and extracts the first line of every roxygen documentation 
#' block (typically the title) along with the name of the function immediately 
#' following that block.
#'
#' @param file_path character string. The file path to the R script you want to parse.
#'
#' @return A data frame with two columns:
#'   * `fn`: The name of the function (character).
#'   * `title`: The extracted first line of the roxygen documentation (character).
#' 
#' @examples
#' \dontrun{
#' roxy_data <- extract_roxygen_info("R/my_functions.R")
#' print(roxy_data)
#' }
#'
#' @export
extractRoxygen <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  
  # Find indices of all lines that start with a roxygen comment
  roxy_idx <- grep("^\\s*#'", lines)
  
  if (length(roxy_idx) == 0) return(data.frame())
  
  # Identify start and end of each roxygen block
  starts <- roxy_idx[!(roxy_idx - 1) %in% roxy_idx]
  ends   <- roxy_idx[!(roxy_idx + 1) %in% roxy_idx]
  
  # Extract and clean titles
  titles <- sub("^\\s*#'\\s*", "", lines[starts])
  
  # Extract function names by looking at the line right after the block ends
  func_names <- sapply(ends, function(idx) {
    # Loop forward to skip any blank lines between the roxygen block and the function
    for (i in (idx + 1):length(lines)) {
      if (grepl("^\\s*$", lines[i])) next # Skip empty lines
      
      # Look for the pattern "functionName <- function" or "functionName = function"
      if (grepl("^\\s*([A-Za-z0-9_.]+)\\s*(<-|=)\\s*function", lines[i])) {
        # Extract just the function name
        return(sub("^\\s*([A-Za-z0-9_.]+)\\s*(<-|=)\\s*function.*", "\\1", lines[i]))
      }
      
      # If we hit a line of code that isn't a function definition, stop looking
      break
    }
    return(NA_character_)
  })
  
  # Create dataframe
  out <- data.frame(fn = unname(func_names), title = titles)

  # Return
  return(out)
}


#' Get the resolution of dates formatted as YYYY-MM-DD, YYYY-MM or YYYY
#'
#' @param x character vector of dates
#'
#' @return character vector, one of "YYYY-MM-DD", "YYYY-MM", "YYYY", or NA
#'     if the date is missing or not in one of these formats
#'
dateRes <- function(x) {
  x <- trimws(as.character(x))
  ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), "YYYY-MM-DD",
    ifelse(grepl("^\\d{4}-\\d{2}$", x), "YYYY-MM",
      ifelse(grepl("^\\d{4}$", x), "YYYY", NA_character_)))
}

#' Translate flags from a plot network into GEO-TREES stem codes
#'
#' @param x character vector of flags, one element per stem
#' @param map named character vector, names are flags in `x`, values are 
#'     GEO-TREES stem codes, see ./templates/stem_codes.csv. Values can 
#'     contain multiple codes, e.g. `c(DC = "DF")`, or be empty, `""`, 
#'     to ignore a flag
#' @param split regular expression used to split `x` into individual flags,
#'     e.g. `"[;, ]+"` for "Q;L", `""` for single character flags, e.g.
#'     "bfj", or NA to use each value of `x` as a single flag, e.g. "dead"
#'
#' @return character vector of GEO-TREES stem codes, NA if no flags map to 
#'     a code
#'
#' @details Flags which are not in `map` give a warning, so that they can be 
#'     added to `map`, either with a GEO-TREES code or `""` to ignore them
#'
codeMap <- function(x, map, split = "") {
  x <- ifelse(is.na(x), "", as.character(x))
  flags <- if (is.na(split)) as.list(x) else strsplit(x, split)
  flags <- lapply(flags, function(y) y[y != ""])

  unknown <- setdiff(unique(unlist(flags)), names(map))
  if (length(unknown) > 0) {
    warning("Flags not in `map`, ignored: ", paste(unknown, collapse = ", "))
  }

  out <- vapply(flags, function(y) {
    paste(map[y[y %in% names(map)]], collapse = "")
  }, character(1))
  out[out == ""] <- NA_character_
  out
}

#' Combine GEO-TREES stem codes into a valid `code` value
#'
#' @param ... character vectors of stem codes, e.g. "A", "BF", combined 
#'     element-wise
#' @param standing logical, recycled to the length of the codes. If TRUE, 
#'     add "S" (standing) to stems which are not "F" (fallen) or "M" (missing)
#'
#' @return character vector of stem codes, with each code once, in the order 
#'     of ./templates/stem_codes.csv. Conflicting codes are resolved: "F" 
#'     (fallen) replaces "S" (standing), "T" (broken below POM) replaces "B" 
#'     (broken above POM), and "M" (missing) removes all codes except "A" 
#'     and "D"
#'
codeCombine <- function(..., standing = FALSE) {
  code_order <- c("A", "D", "S", "F", "M", "B", "T", "U", "I", "H")
  x <- pasteVals(..., sep = "")
  standing <- rep_len(standing, length(x))

  out <- vapply(seq_along(x), function(i) {
    y <- unique(strsplit(ifelse(is.na(x[i]), "", x[i]), "")[[1]])
    if ("F" %in% y) y <- setdiff(y, "S")
    if ("T" %in% y) y <- setdiff(y, "B")
    if ("M" %in% y) y <- intersect(y, c("A", "D", "M"))
    if (standing[i] && !any(c("F", "M") %in% y)) y <- c(y, "S")
    paste(code_order[code_order %in% y], collapse = "")
  }, character(1))
  out[out == ""] <- NA_character_
  out
}

#' Format plot corner coordinates as a GEO-TREES plot geo-location table
#'
#' @param x dataframe with one row per point and columns: `site_id`, 
#'     `plot_id`, `point_id`, `x_rel_m`, `y_rel_m`, and either `longitude`
#'     and `latitude` in WGS84 decimal degrees, or `rover_easting_utm_m`, 
#'     `rover_northing_utm_m` and `crs_epsg`. Other columns in `cols` are kept
#' @param cols column spec table, ./templates/pt_cols.csv
#'
#' @return dataframe with the columns in `cols`. Longitude and latitude are
#'     transformed to the UTM zone containing most points. `corner` is TRUE 
#'     if not provided
#'
ptFormat <- function(x, cols) {
  x <- as.data.frame(x)

  if (all(c("longitude", "latitude") %in% names(x))) {
    ok <- !is.na(x$longitude) & !is.na(x$latitude)
    epsg <- utmZone(x$longitude[ok], x$latitude[ok], epsg = TRUE)
    zone <- utmZone(x$longitude[ok], x$latitude[ok], epsg = FALSE)
    crs <- as.integer(names(which.max(table(epsg))))
    xy <- sf::st_coordinates(sf::st_transform(
      sf::st_as_sf(x[ok, c("longitude", "latitude")], 
        coords = c("longitude", "latitude"), crs = 4326), crs))
    x$rover_easting_utm_m <- NA_real_
    x$rover_northing_utm_m <- NA_real_
    x$rover_easting_utm_m[ok] <- xy[, 1]
    x$rover_northing_utm_m[ok] <- xy[, 2]
    x$crs_epsg <- crs
    x$crs_name <- paste("UTM", zone[match(crs, epsg)])
  } else if (!all(c("rover_easting_utm_m", "rover_northing_utm_m", "crs_epsg") %in% names(x))) {
    stop("`x` must contain either `longitude` and `latitude`, or ",
      "`rover_easting_utm_m`, `rover_northing_utm_m` and `crs_epsg`")
  }

  if (!"corner" %in% names(x)) {
    x$corner <- TRUE
  }
  x$crs_epsg <- as.integer(x$crs_epsg)

  x <- suppressMessages(colGen(x, cols$column_name, cols$class))

  # Convert columns to the classes in `cols`
  for (i in seq_len(nrow(cols))) {
    if (!inherits(x[[cols$column_name[i]]], cols$class[i])) {
      x[[cols$column_name[i]]] <- match.fun(paste0("as.", cols$class[i]))(x[[cols$column_name[i]]])
    }
  }

  x[, cols$column_name]
}

#' Convert YYYY-MM-DD, YYYY-MM or YYYY dates to decimal years
#'
#' Partial dates are placed at the midpoint of their month or year
#'
#' @param x character vector of dates
#' @param digits number of decimal places to round to. If NULL, no rounding
#'
#' @return numeric vector of decimal years
#'
decDate <- function(x, digits = 3) {
  x <- trimws(as.character(x))

  res <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), "YYYY-MM-DD",
    ifelse(grepl("^\\d{4}-\\d{2}$", x), "YYYY-MM",
      ifelse(grepl("^\\d{4}$", x), "YYYY", NA_character_)))

  y <- as.integer(substr(x, 1, 4))
  m <- as.integer(substr(x, 6, 7))

  # Start and (exclusive) end of the interval covered by each date
  start <- as.Date(ifelse(res == "YYYY-MM-DD", x,
    ifelse(res == "YYYY-MM", paste0(x, "-01"),
      ifelse(res == "YYYY", paste0(x, "-01-01"), NA_character_))))
  end <- as.Date(ifelse(res == "YYYY-MM-DD", x,
    ifelse(res == "YYYY-MM", sprintf("%04d-%02d-01", y + m %/% 12, m %% 12 + 1),
      ifelse(res == "YYYY", sprintf("%04d-01-01", y + 1), NA_character_))))

  # Midpoint of interval
  mid <- start + (end - start) / 2

  # Fraction of year elapsed at midpoint
  y_mid <- as.integer(format(mid, "%Y"))
  y_start <- as.Date(paste0(y_mid, "-01-01"))
  y_len <- as.numeric(as.Date(paste0(y_mid + 1, "-01-01")) - y_start)
  dec <- y_mid + as.numeric(mid - y_start) / y_len

  # Optionally round
  if (!is.null(digits)) {
    dec <- round(dec, digits)
  }

  return(dec)
}


#' Colours used in figures to check processing outputs
#'
#' `series` are categorical colours, used in this order. `seq` is a 
#' single-hue ramp for magnitudes, light to dark. `seq2` is a second 
#' single-hue ramp, for a second magnitude shown alongside the first, e.g.
#' uncertainty. `problem` and `warn` mark 
#' data which may need correcting, e.g. outliers, and are always paired with 
#' a label in the legend
#'
fig_pal <- list(
  series = c("#2a78d6", "#eb6834", "#1baf7a"),
  seq = c("#cde2fb", "#86b6ef", "#3987e5", "#256abf", "#184f95", "#0d366b"),
  seq2 = c("#fde3d6", "#f8b896", "#f28c5f", "#eb6834", "#c24e1f", "#8c3312"),
  problem = "#d03b3b",
  warn = "#ec835a",
  ink = "#0b0b0b",
  ink_muted = "#52514e",
  grid = "#e6e5e1",
  line_muted = "#b8b7b2",
  surface = "#fcfcfb")

#' ggplot2 theme for figures to check processing outputs
#'
#' @param base_size base font size
#'
#' @return ggplot2 theme
#'
themeFig <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = fig_pal$surface, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = fig_pal$grid, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = fig_pal$ink_muted),
      axis.title = ggplot2::element_text(colour = fig_pal$ink_muted),
      plot.title = ggplot2::element_text(colour = fig_pal$ink, face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = fig_pal$ink_muted),
      plot.title.position = "plot",
      strip.text = ggplot2::element_text(colour = fig_pal$ink, hjust = 0, face = "bold"),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_text(colour = fig_pal$ink_muted),
      legend.text = ggplot2::element_text(colour = fig_pal$ink))
}

#' ggplot2 theme for maps of plots, without grid lines, so that quadrat 
#' grids are visible
#'
#' @param base_size base font size
#'
#' @return ggplot2 theme
#'
themeMap <- function(base_size = 12) {
  themeFig(base_size) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank())
}

#' Order plot IDs for figures, numerically if all IDs are numbers
#'
#' @param x vector of plot IDs
#'
#' @return factor with levels in order
#'
plotFactor <- function(x) {
  x <- as.character(x)
  lev <- unique(x)
  num <- suppressWarnings(as.numeric(lev))
  lev <- if (all(!is.na(num))) lev[order(num)] else sort(lev)
  factor(x, levels = lev)
}

#' Number of facet columns for a figure with small multiples
#'
#' @param n number of panels
#'
#' @return integer, number of columns
#'
figCols <- function(n) {
  as.integer(max(1, ceiling(sqrt(n))))
}

#' Save a figure to check processing outputs
#'
#' The figure is sized to fit the number of panels, laid out with 
#' `figCols()` columns, with a minimum size so that the legend and titles 
#' fit. Saved at 300 dpi. Text in figures wider than 8 inches is enlarged, 
#' so that it is readable when the whole figure is viewed on screen
#'
#' @param p ggplot object
#' @param file path to output PNG file
#' @param n_panel number of panels in the figure
#' @param panel_in width and height of each panel, in inches
#' @param width optional figure width in inches, overrides `n_panel`
#' @param height optional figure height in inches, overrides `n_panel`
#' @param base_size base font size of the figure theme, see `themeFig()`
#'
#' @return path to output file, invisibly
#'
saveFig <- function(p, file, n_panel = 1, panel_in = 3.4, width = NULL, 
  height = NULL, base_size = 12) {
  n_col <- figCols(n_panel)
  n_row <- ceiling(n_panel / n_col)
  if (is.null(width)) width <- min(max(1.4 + panel_in * n_col, 8), 24)
  if (is.null(height)) height <- min(max(2 + panel_in * n_row, 5), 24)
  if (width > 8) {
    p <- p + ggplot2::theme(text = ggplot2::element_text(
      size = base_size * sqrt(width / 8)))
  }
  ggplot2::ggsave(file, p, width = width, height = height, dpi = 300, 
    bg = fig_pal$surface)
  message("Figure written: ", file)
  invisible(file)
}

#' Print a table to the console as a message
#'
#' @param x dataframe
#' @param title optional title printed above the table
#' @param max_rows maximum number of rows to print
#' @param digits number of significant digits for numeric columns
#'
#' @return NULL, invisibly
#'
msgTable <- function(x, title = NULL, max_rows = 30, digits = 3) {
  x <- as.data.frame(x)
  n <- nrow(x)
  # Print wide tables without wrapping columns
  op <- options(width = 250, max.print = 99999)
  on.exit(options(op))
  out <- utils::capture.output(print(utils::head(x, max_rows),
    row.names = FALSE, digits = digits))
  if (n > max_rows) {
    out <- c(out, paste0("... ", n - max_rows, " more rows"))
  }
  message(paste(c(title, out), collapse = "\n"))
  invisible(NULL)
}

#' Shift coordinates of each plot to a local origin, for maps of plots
#'
#' @param x dataframe with coordinates
#' @param x_col,y_col names of coordinate columns
#' @param group_col name of plot ID column
#' @param origin optional dataframe of origins, with columns `group_col`, 
#'     `x0` and `y0`. If NULL, the minimum coordinates of each plot are used
#'
#' @return `x` with coordinates relative to the origin of each plot
#'
localXY <- function(x, x_col, y_col, group_col, origin = NULL) {
  if (is.null(origin)) {
    origin <- data.frame(
      id = unique(as.character(x[[group_col]])))
    origin$x0 <- vapply(origin$id, function(g) {
      min(x[[x_col]][as.character(x[[group_col]]) == g], na.rm = TRUE)
    }, numeric(1))
    origin$y0 <- vapply(origin$id, function(g) {
      min(x[[y_col]][as.character(x[[group_col]]) == g], na.rm = TRUE)
    }, numeric(1))
    names(origin)[1] <- group_col
  }
  i <- match(as.character(x[[group_col]]), as.character(origin[[group_col]]))
  x[[x_col]] <- x[[x_col]] - origin$x0[i]
  x[[y_col]] <- x[[y_col]] - origin$y0[i]
  x
}

#' Extract polygon vertices as a dataframe, for maps of plots
#'
#' @param x sf object of POLYGON geometries
#' @param id_col name of plot ID column in `x`
#'
#' @return dataframe with columns: `X`, `Y`, `plot_id`, and `group`, which
#'     identifies each polygon
#'
polyCoords <- function(x, id_col = "plot_id") {
  xy <- as.data.frame(sf::st_coordinates(x))
  xy$plot_id <- as.character(x[[id_col]])[xy$L2]
  xy$group <- paste(xy$plot_id, xy$L2)
  xy
}
