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

#' Check field subplot bounds table values
#'
#' Runs various checks on the values in field subplot bounds table columns
#'
#' @param x dataframe containing field subplot bounds, see 
#'     ./templates/subplot_cols.csv
#' @param cols optional column spec table, used to check minimum and maximum
#'     values
#'
subplotValCheck <- function(x, cols = NULL) {
  # Only one site ID per site
  if (length(unique(x$site_id)) > 1) {
    stop("`site_id` must be the same for all plots within a site")
  }

  # Plot and subplot IDs must identify each subplot
  if (any(is.na(x$plot_id) | x$plot_id == "" | 
    is.na(x$subplot_id) | x$subplot_id == "")) {
    stop("`plot_id` and `subplot_id` must not be NA in 'subplot'")
  }
  if (any(duplicated(x[, c("plot_id", "subplot_id")]))) {
    stop("Each combination of `plot_id` and `subplot_id` must be unique in 'subplot'")
  }

  # Bounds must be complete, and enclose an area
  bound_cols <- c("x_rel_min_m", "x_rel_max_m", "y_rel_min_m", "y_rel_max_m")
  if (any(is.na(x[, bound_cols]))) {
    stop("Subplot bounds must not be NA in 'subplot'")
  }
  bad <- x$x_rel_max_m <= x$x_rel_min_m | x$y_rel_max_m <= x$y_rel_min_m
  if (any(bad)) {
    stop(sum(bad), " subplots in 'subplot' have a maximum X or Y coordinate ",
      "which is not larger than the minimum")
  }

  # Values within ranges defined in spec table
  rangeCheck(x, cols, "subplot")

  # Subplots in the same plot should not overlap
  n_overlap <- sum(vapply(split(x, x$plot_id), function(y) {
    if (nrow(y) < 2) return(0L)
    ov <- outer(y$x_rel_min_m, y$x_rel_max_m, "<") & 
      outer(y$x_rel_max_m, y$x_rel_min_m, ">") &
      outer(y$y_rel_min_m, y$y_rel_max_m, "<") & 
      outer(y$y_rel_max_m, y$y_rel_min_m, ">")
    sum(ov[upper.tri(ov)])
  }, integer(1)))
  if (n_overlap > 0) {
    warning(n_overlap, " pairs of subplots in 'subplot' overlap within a plot",
      call. = FALSE)
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
#' @param subplot field subplot bounds table
#' @param cols optional named list of column spec tables, with names
#'     `plot`, `stem`, `pt`, `height`, `dup`, `subplot`. If NULL, objects 
#'     named `plot_cols`, `stem_cols`, `pt_cols`, `height_cols`, 
#'     `stem_dup_cols`, `subplot_cols` are used if they exist where 
#'     `valCheck()` is called from
#'
valCheck <- function(plot = NULL, stem = NULL, pt = NULL, taxon = NULL, 
  height = NULL, wd = NULL, dup = NULL, subplot = NULL, cols = NULL) { 

  # Find column spec tables
  if (is.null(cols)) {
    env <- parent.frame()
    cols <- lapply(c(plot = "plot", stem = "stem", pt = "pt", height = "height",
      dup = "stem_dup", subplot = "subplot"), 
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

  if (!is.null(subplot)) {
    subplotValCheck(subplot, cols$subplot)
  }

  # All tables must have the same site ID
  site_ids <- unique(unlist(lapply(list(plot, stem, pt, taxon, height, wd, dup, subplot), 
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

  # Run tests to match the subplot table to other tables
  if (!is.null(subplot) && !is.null(plot)) {
    # All plots in subplot must be in plot
    if (!all(subplot$plot_id %in% plot$plot_id)) {
      stop("Some plots in 'subplot' are missing from 'plot'")
    }
  }

  if (!is.null(subplot) && !is.null(stem)) {
    # Field subplots of stems should be in subplot, in plots which have
    # subplots in subplot
    s <- stem[!is.na(stem$subplot_id) & stem$subplot_id != "" & 
      stem$plot_id %in% subplot$plot_id, ]
    miss <- unique(paste(s$plot_id, s$subplot_id))[
      !unique(paste(s$plot_id, s$subplot_id)) %in% 
        paste(subplot$plot_id, subplot$subplot_id)]
    if (length(miss) > 0) {
      warning(length(miss), " field subplots in 'stem' are missing from ",
        "'subplot': ", paste(utils::head(miss, 5), collapse = ", "),
        if (length(miss) > 5) ", ...", call. = FALSE)
    }
    # Plots with field subplots in 'stem' but not in subplot
    plot_miss <- setdiff(unique(stem$plot_id[!is.na(stem$subplot_id) & 
      stem$subplot_id != ""]), subplot$plot_id)
    if (length(plot_miss) > 0) {
      warning(length(plot_miss), " plots with field subplots in 'stem' are ",
        "missing from 'subplot': ", paste(plot_miss, collapse = ", "),
        call. = FALSE)
    }
  }

  if (!is.null(subplot) && !is.null(pt)) {
    # Subplots should be within the extent of the plot corners, with 1% 
    # tolerance
    corner <- pt[pt$corner %in% TRUE & !is.na(pt$x_rel_m) & !is.na(pt$y_rel_m), ]
    ext <- do.call(rbind, lapply(split(corner, corner$plot_id), function(y) {
      data.frame(
        plot_id = y$plot_id[1],
        x_min = min(y$x_rel_m), x_max = max(y$x_rel_m),
        y_min = min(y$y_rel_m), y_max = max(y$y_rel_m))
    }))
    e <- ext[match(subplot$plot_id, ext$plot_id), ]
    tol_x <- (e$x_max - e$x_min) * 0.01
    tol_y <- (e$y_max - e$y_min) * 0.01
    out <- subplot$x_rel_min_m < e$x_min - tol_x | 
      subplot$x_rel_max_m > e$x_max + tol_x |
      subplot$y_rel_min_m < e$y_min - tol_y | 
      subplot$y_rel_max_m > e$y_max + tol_y
    if (any(out, na.rm = TRUE)) {
      warning(sum(out, na.rm = TRUE), " subplots in 'subplot' extend outside ",
        "the extent of their plot corners", call. = FALSE)
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
#' Selects stems which are alive, standing, and are not broken below the POM
#' or missing. Where a stem was measured more than once, e.g. at different 
#' POMs, the measurement with a diameter, the highest POM, latest date, and 
#' largest diameter is used. Stems without coordinates, and stems below the
#' minimum diameter threshold of the plot, are included. See `agbdFilter()`
#' for stems used to estimate AGB density, and to form multi-stemmed trees.
#'
#' @param stem stem measurement table, see ./templates/stem_cols.csv
#' @param plot plot meta-data table, see ./templates/plot_cols.csv
#' @param missing_diam logical, if TRUE stems without a diameter measurement
#'     are included, and their diameters are imputed during AGB error 
#'     propagation in ./08_agb_mc.R. See `missing_diam_method` in param.yaml
#'
#' @return character vector of `record_id` values
#'
recordFilter <- function(stem, plot, missing_diam = FALSE) {
  stem %>%
    filter(
      !is.na(diam_cm) | missing_diam,
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
      order_by = tibble(!is.na(diam_cm), pom_m, measurement_date, diam_cm),
      n = 1,
      with_ties = FALSE) %>%
    ungroup() %>%
    pull(record_id) %>%
    as.character()
}

#' Select stem measurements used to estimate AGB density
#'
#' Selects stems from `recordFilter()` with a diameter at or above the minimum
#' diameter threshold, and optionally excludes stems broken above the POM, and
#' stems of some growth forms. Other stems have their own AGB estimates, but
#' are not part of multi-stemmed trees, and are not included in AGB density,
#' so that AGB density matches the sampling of the plot. Stems without a
#' diameter measurement, if included by `recordFilter()`, are assumed to be at
#' or above the threshold, as they were recorded in the census. The threshold
#' applies to the measured diameter, `diam_cm_meas` if `stem` has been passed
#' through `taperDiam()`, as the plot was sampled on measured diameters.
#'
#' @param stem stem measurement table, see ./templates/stem_cols.csv
#' @param plot plot meta-data table, see ./templates/plot_cols.csv
#' @param record_fil `record_id` values from `recordFilter()`
#' @param diam_min_cm minimum diameter threshold for AGB density. The larger
#'     of this and the minimum diameter threshold of the plot is used
#' @param include_broken logical, if FALSE stems broken above the POM ("B" in
#'     `code`) are excluded
#' @param growth_form_exclude optional character vector of values of
#'     `growth_form` in the stem table to exclude, e.g. "strangler"
#'
#' @return character vector of `record_id` values
#'
agbdFilter <- function(stem, plot, record_fil, diam_min_cm = 0,
  include_broken = TRUE, growth_form_exclude = NULL) {
  diam_min <- plot$meas_diam_min_cm[match(stem$plot_id, plot$plot_id)]
  diam_min <- pmax(diam_min, diam_min_cm)
  diam <- if ("diam_cm_meas" %in% names(stem)) stem$diam_cm_meas else stem$diam_cm
  keep <- as.character(stem$record_id) %in% as.character(record_fil) &
    !is.na(diam_min) & (is.na(diam) | diam >= diam_min)
  if (!include_broken) {
    keep <- keep & !grepl("B", stem$code)
  }
  if (length(growth_form_exclude) > 0) {
    keep <- keep & !stem$growth_form %in% growth_form_exclude
  }
  as.character(stem$record_id[keep])
}

#' Taper parameter of the exponential taper model
#'
#' The taper model, d = D exp(-b (h - 1.3)), gives the diameter d at height h
#' (m) of a stem with diameter D at 1.3 m (Metcalf et al. 2008; Cushman et al.
#' 2014). This function predicts b for a diameter measured at height `hom`.
#'
#' @param diam diameter (cm) measured at `hom`, i.e. diameter above buttress
#' @param hom height of the diameter measurement (m)
#' @param method "cushman2014", the model for Barro Colorado Island, Panama,
#'     of Cushman et al. (2014), Table 3: log(b) = -2.0205 - 0.5053 log(D) +
#'     0.3748 log(hom), plus a species term. Or "cushman2021", the general
#'     model of Cushman et al. (2021), Table 2, Model 1, fitted at five
#'     lowland tropical forest sites: b = 0.151 - 0.025 log(D) -
#'     0.02 log(hom) - 0.021 log(wd), without the site effect. Logs are
#'     natural logs
#' @param wd wood density (g cm-3), used by "cushman2021"
#' @param sp_term species term of "cushman2014", 0 for species without a term
#'
#' @return vector of b. For "cushman2021", b is at least 0, as the model can
#'     give b < 0 for very large trees measured high on the stem, which would
#'     make the diameter at 1.3 m smaller than the measured diameter
#'
taperB <- function(diam, hom, method, wd = NULL, sp_term = 0) {
  if (method == "cushman2014") {
    exp(-2.0205 + sp_term - 0.5053 * log(diam) + 0.3748 * log(hom))
  } else if (method == "cushman2021") {
    pmax(0.151 - 0.025 * log(diam) - 0.02 * log(hom) - 0.021 * log(wd), 0)
  } else {
    stop("Unknown taper method: ", method)
  }
}

#' Use taper-corrected diameters to estimate AGB
#'
#' Replaces `diam_cm` in the stem table with the diameter at 1.3 m corrected
#' for taper, `diam_cm_taper` from ./04_height.R, where it exists. The measured
#' diameter is kept in `diam_cm_meas`, which `agbdFilter()` uses for diameter
#' thresholds.
#'
#' @param stem stem measurement table
#' @param stem_height stem height table from ./04_height.R, with `record_id`
#'     and `diam_cm_taper`
#'
#' @return `stem`, with `diam_cm` replaced and `diam_cm_meas` added
#'
taperDiam <- function(stem, stem_height) {
  stem$diam_cm_meas <- stem$diam_cm
  if ("diam_cm_taper" %in% names(stem_height)) {
    d <- stem_height$diam_cm_taper[match(stem$record_id, stem_height$record_id)]
    stem$diam_cm <- ifelse(is.na(d), stem$diam_cm, d)
  }
  stem
}

#' AGB allometries which can be assigned to individual stems
#'
#' Allometries other than Chave et al. (2014), which can be assigned to stems
#' in `agb_allometry` in the stem table, used if `stem_allometry` is true in
#' param.yaml. Each is a list in the format of the `fitted_allom` argument of
#' BIOMASS::AGBmonteCarlo(): a one-sided `formula` of D (cm), H (m) and/or WD
#' (g cm-3) giving AGB in Mg, or log(AGB) if `link` is "log", a data frame of
#' `parameters` including the residual standard deviation `sigma`, and the
#' `link`. Stems with `agb_allometry` "chave2014" or NA use Chave et al.
#' (2014), Eq. 4 or Eq. 7, set by `height_method`.
#'
agb_allometries <- list(
  # Goodman et al. (2013), Arecaceae, diameter only:
  # ln(AGB kg) = -3.3488 + 2.7483 ln(D), RSE 0.5876
  goodman2013 = list(
    formula = ~ a + b * log(D),
    parameters = data.frame(a = -3.3488 - log(1000), b = 2.7483, sigma = 0.5876),
    link = "log",
    description = paste0("Goodman et al. (2013) Arecaceae allometry, ",
      "ln(AGB kg) = -3.3488 + 2.7483 ln(D)")))

#' Mean AGB from an allometry in `agb_allometries`
#'
#' @param allom an element of `agb_allometries`
#' @param D diameter (cm)
#' @param H height (m)
#' @param WD wood density (g cm-3)
#'
#' @return vector of AGB (Mg). With a log link, the mean of the log-normal,
#'     exp(mu + sigma^2 / 2), consistent with the mean of simulations in
#'     BIOMASS::AGBmonteCarlo()
#'
agbAllomMean <- function(allom, D, H = NULL, WD = NULL) {
  p <- as.list(allom$parameters[1, , drop = FALSE])
  mu <- eval(allom$formula[[2]], c(list(D = D, H = H, WD = WD), p))
  if (identical(allom$link, "log")) exp(mu + p$sigma^2 / 2) else mu
}

#' Height-diameter model from param.yaml
#'
#' @param x `height_custom` from param.yaml: a list with `formula`, a string
#'     giving height (m) as a function of diameter `D` (cm), e.g.
#'     "a * D^b / (k + D^b)", `parameters`, a named list of parameter values,
#'     `sigma`, the residual standard deviation, `link`, "log" if the
#'     residual error is log-normal and `formula` gives the median height, or
#'     "identity", and `reference`
#'
#' @return a height-diameter model from BIOMASS::customHD()
#'
heightCustom <- function(x) {
  BIOMASS::customHD(
    formula = as.formula(paste("~", x$formula)),
    parameters = unlist(x$parameters),
    sigma = x$sigma,
    link = if (is.null(x$link)) "log" else x$link,
    name = x$reference)
}

#' Crown area allometry from param.yaml
#'
#' @param x `crown_custom` from param.yaml: a list with `formula`, a string
#'     giving crown area (m2) as a function of diameter `D` (cm), e.g.
#'     "a * D^b", `parameters`, a named list of parameter values, `sigma`,
#'     the residual standard deviation, `link`, "log" if the residual error
#'     is log-normal and `formula` gives the median crown area, or
#'     "identity", and `reference`
#'
#' @return list with `formula` (an expression), `parameters`, `sigma`, `link`
#'     and `reference`, used by `crownArea()`
#'
crownCustom <- function(x) {
  if (is.null(x$formula) || is.null(x$parameters) || is.null(x$sigma)) {
    stop("`crown_custom` must give the crown area allometry's `formula`, ",
      "`parameters` and `sigma`")
  }
  if (!all(vapply(x$parameters, is.numeric, logical(1))) ||
    is.null(names(unlist(x$parameters)))) {
    stop("`parameters` of `crown_custom` must be named numbers, e.g. ",
      "{a: 0.57, b: 1.34}")
  }
  if (length(x$sigma) != 1 || !is.numeric(x$sigma) || x$sigma < 0) {
    stop("`sigma` of `crown_custom` must be a number >= 0")
  }
  link <- if (is.null(x$link)) "log" else x$link
  if (!link %in% c("log", "identity")) {
    stop("`link` of `crown_custom` must be \"log\" or \"identity\"")
  }
  formula <- str2lang(x$formula)
  vars_unknown <- setdiff(all.vars(formula), c("D", names(unlist(x$parameters))))
  if (length(vars_unknown) > 0) {
    stop("`formula` of `crown_custom` uses variables which are not `D` or ",
      "in `parameters`: ", paste(vars_unknown, collapse = ", "))
  }
  out <- list(
    formula = formula,
    parameters = as.list(unlist(x$parameters)),
    sigma = x$sigma,
    link = link,
    reference = if (is.null(x$reference)) "" else x$reference)
  # Check the formula can be evaluated
  ca <- crownArea(out, c(10, 50))
  if (any(!is.finite(ca)) || any(ca <= 0)) {
    stop("`crown_custom` must give a positive crown area for D = 10 and 50 cm")
  }
  out
}

#' Crown area from a crown area allometry
#'
#' @param model crown area allometry from `crownCustom()`
#' @param D diameter (cm)
#' @param z standard normal residual draws, the same length as `D`, or 0 for
#'     the median crown area with a log link
#'
#' @return vector of crown area (m2), at least 0
#'
crownArea <- function(model, D, z = 0) {
  ca <- eval(model$formula, c(list(D = D), model$parameters))
  ca <- if (model$link == "log") ca * exp(model$sigma * z) else ca + model$sigma * z
  pmax(ca, 0)
}

#' Points on the unit disc used to split crowns among quadrats
#'
#' Points are in `n_ring` rings of equal area, with `n_angle` points in each
#' ring, so each point represents the same area of the crown. Points in
#' alternate rings are staggered, and no point lies on the x or y axis, so
#' that a crown centred on a quadrat edge is split equally
#'
#' @param n_ring number of rings
#' @param n_angle number of points in each ring
#'
#' @return list with `u` and `v`, coordinates of the points on the unit disc,
#'     and `w`, the weight of each point, summing to 1
#'
crownTemplate <- function(n_ring = 16, n_angle = 32) {
  rho <- sqrt((seq_len(n_ring) - 0.5) / n_ring)
  g <- expand.grid(a = seq_len(n_angle), k = seq_len(n_ring))
  theta <- 2 * pi * (g$a - 0.75 + 0.5 * (g$k %% 2)) / n_angle
  list(u = rho[g$k] * cos(theta), v = rho[g$k] * sin(theta),
    w = rep(1 / nrow(g), nrow(g)))
}

#' Regular grid of quadrats in the relative coordinates of a plot
#'
#' @param x quadrat corners of one plot, with columns `quadrat_id`,
#'     `x_rel_m` and `y_rel_m`, e.g. from ./07_quad.R
#'
#' @return list with the grid origin `x0` and `y0`, quadrat dimensions `qx`
#'     and `qy`, numbers of quadrats `nx` and `ny`, and `quad`, a dataframe
#'     with one row per quadrat in grid order, `ix + iy * nx + 1`, with its
#'     extent. NULL if the quadrats are not a complete regular grid
#'
quadGrid <- function(x) {
  quad <- x %>%
    group_by(quadrat_id) %>%
    summarise(
      x_min = min(x_rel_m), x_max = max(x_rel_m),
      y_min = min(y_rel_m), y_max = max(y_rel_m),
      .groups = "drop")
  g <- list(
    x0 = min(quad$x_min),
    y0 = min(quad$y_min),
    qx = median(quad$x_max - quad$x_min),
    qy = median(quad$y_max - quad$y_min))
  quad$ix <- as.integer(round((quad$x_min - g$x0) / g$qx))
  quad$iy <- as.integer(round((quad$y_min - g$y0) / g$qy))
  g$nx <- max(quad$ix) + 1L
  g$ny <- max(quad$iy) + 1L
  quad$quad <- quad$ix + quad$iy * g$nx + 1L
  tol <- 0.01
  if (nrow(quad) != g$nx * g$ny || anyDuplicated(quad$quad) > 0 ||
    any(abs(quad$x_max - quad$x_min - g$qx) > tol) ||
    any(abs(quad$y_max - quad$y_min - g$qy) > tol) ||
    any(abs(quad$x_min - g$x0 - quad$ix * g$qx) > tol) ||
    any(abs(quad$y_min - g$y0 - quad$iy * g$qy) > tol)) {
    return(NULL)
  }
  g$quad <- quad[order(quad$quad), ]
  g
}

#' Allocate tree AGB to quadrats by crown
#'
#' Each tree's crown is a circle of radius `r` centred on the tree. Crowns
#' inside the home quadrat of the tree are assigned to it. Crowns crossing a
#' quadrat edge are split by the share of the points of `tmpl` in each
#' quadrat. The part of a crown outside the quadrat grid is folded back into
#' the part inside the grid, so the total AGB of the grid is the same as with
#' each tree in its home quadrat. Trees whose crown has no points inside the
#' grid stay in their home quadrat.
#'
#' @param x,y tree coordinates, in the relative coordinates of the plot
#' @param agb tree AGB
#' @param r crown radius
#' @param home index of the home quadrat of each tree in `grid$quad`
#' @param grid quadrat grid from `quadGrid()`
#' @param tmpl crown points from `crownTemplate()`
#'
#' @return vector of AGB in each quadrat, in the order of `grid$quad`
#'
crownAlloc <- function(x, y, agb, r, home, grid, tmpl) {
  nq <- nrow(grid$quad)
  sumQuad <- function(q, a) {
    out <- numeric(nq)
    if (length(q) > 0) {
      s <- rowsum(a, q, reorder = FALSE)
      out[as.integer(rownames(s))] <- s[, 1]
    }
    out
  }
  hq <- grid$quad[home, ]
  d_edge <- pmin(x - hq$x_min, hq$x_max - x, y - hq$y_min, hq$y_max - y)
  cross <- r > d_edge
  out <- sumQuad(home[!cross], agb[!cross])
  if (any(cross)) {
    jx <- floor((x[cross] + outer(r[cross], tmpl$u) - grid$x0) / grid$qx)
    jy <- floor((y[cross] + outer(r[cross], tmpl$v) - grid$y0) / grid$qy)
    inside <- jx >= 0 & jx < grid$nx & jy >= 0 & jy < grid$ny
    w <- matrix(tmpl$w, nrow = sum(cross), ncol = length(tmpl$w), byrow = TRUE) *
      inside
    w_in <- rowSums(w)
    agb_cross <- agb[cross]
    # Crowns with no points inside the grid stay in their home quadrat
    none <- w_in == 0
    out <- out + sumQuad(home[cross][none], agb_cross[none])
    w <- w / ifelse(none, 1, w_in)
    out <- out + sumQuad((jx + jy * grid$nx + 1)[inside],
      (w * agb_cross)[inside])
  }
  out
}

#' Format a parameter from param.yaml as a single value
#'
#' Parameters with several values, or named values, e.g. `growth_form_exclude`
#' or `height_custom`, are collapsed to one string, so that each parameter
#' is one column in the L3 product
#'
#' @param x value of a parameter
#'
#' @return `x`, or a string
#'
paramString <- function(x) {
  if (!is.list(x) && length(x) <= 1 && is.null(names(x))) {
    return(x)
  }
  x <- unlist(x)
  if (is.null(names(x))) {
    paste(x, collapse = ", ")
  } else {
    paste(names(x), x, sep = " = ", collapse = "; ")
  }
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

#' Convert decimal years to dates
#'
#' Inverse of `decDate()`. Decimal years are rounded to the nearest day
#'
#' @param x numeric vector of decimal years
#'
#' @return Date vector
#'
decToDate <- function(x) {
  x[!is.finite(x)] <- NA_real_
  y <- floor(x)
  y_start <- as.Date(ifelse(is.na(y), NA_character_, sprintf("%04d-01-01", y)))
  y_len <- as.numeric(as.Date(ifelse(is.na(y), NA_character_,
    sprintf("%04d-01-01", y + 1))) - y_start)
  y_start + round((x - y) * y_len)
}

#' Format decimal years as dates at a given resolution
#'
#' @param x numeric vector of decimal years
#' @param res resolution of the dates: "YYYY-MM-DD", "YYYY-MM" or "YYYY".
#'     Recycled to the length of `x`
#'
#' @return character vector of dates
#'
decFormat <- function(x, res = "YYYY-MM-DD") {
  fmt <- c("YYYY-MM-DD" = "%Y-%m-%d", "YYYY-MM" = "%Y-%m", "YYYY" = "%Y")
  res <- rep_len(res, length(x))
  res_fmt <- ifelse(res %in% names(fmt), fmt[res], "%Y-%m-%d")
  d <- decToDate(x)
  vapply(seq_along(x), function(i) {
    if (is.na(d[i])) NA_character_ else format(d[i], res_fmt[i])
  }, character(1))
}

#' Read an optional CSV file
#'
#' @param file path to CSV file
#' @param ... further arguments passed to `read.csv()`
#'
#' @return dataframe, or NULL if `file` does not exist
#'
readOpt <- function(file, ...) {
  if (file.exists(file)) read.csv(file, ...) else NULL
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

#' Colour bar legend, the same shape in all figures
#'
#' A wide bar with the title above it, so that the labels of long scales do
#'     not overlap
#'
#' @return a `ggplot2::guide_colourbar()` object, passed to `guide` of a
#'     continuous colour or fill scale
#'
colourBar <- function() {
  ggplot2::guide_colourbar(theme = ggplot2::theme(
    legend.title.position = "top",
    legend.key.width = ggplot2::unit(16, "lines"),
    legend.key.height = ggplot2::unit(0.8, "lines")))
}

#' Width of a figure with panels laid out with `figCols()` columns
#'
#' @param n_panel number of panels in the figure
#' @param panel_in width of each panel, in inches
#'
#' @return figure width in inches, between 8 and 24. Allows 1.4 inches for
#'     the axis title and labels, so each panel is about
#'     `(width - 1.4) / figCols(n_panel)` inches wide
#'
figWidth <- function(n_panel = 1, panel_in = 3.4) {
  min(max(1.4 + panel_in * figCols(n_panel), 8), 24)
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
  if (is.null(width)) width <- figWidth(n_panel, panel_in)
  if (is.null(height)) height <- min(max(2 + panel_in * n_row, 5), 24)
  text_size <- base_size
  if (width > 8) {
    text_size <- base_size * sqrt(width / 8)
    p <- p + ggplot2::theme(text = ggplot2::element_text(size = text_size))
  }
  # Wrap long subtitles to the width of the figure. Characters are about
  # 0.55 times the font size wide
  if (is.character(p$labels$subtitle)) {
    n_char <- floor((width - 0.5) * 72 / (0.55 * text_size))
    p$labels$subtitle <- paste(strwrap(p$labels$subtitle, width = n_char),
      collapse = "\n")
  }
  ggplot2::ggsave(file, p, width = width, height = height, dpi = 300,
    bg = fig_pal$surface)
  message("Figure written: ", file)
  invisible(file)
}

#' Histograms of wood density estimates at each taxonomic level
#'
#' One panel per level, labelled with the number and percentage of stems at
#' that level. Y-axis scales differ between panels
#'
#' @param wd numeric vector of wood density estimates, g cm-3
#' @param level taxonomic level of each estimate, e.g. "species", "genus".
#'     Panels are in the order of the levels if a factor
#'
#' @return ggplot object
#'
wdLevelPlot <- function(wd, level) {
  level <- droplevels(as.factor(level))
  n <- table(level)
  lab <- stats::setNames(paste0(names(n), " (",
    format(as.vector(n), big.mark = ",", trim = TRUE), " stems, ",
    round(100 * as.vector(n) / sum(n), 1), "%)"), names(n))
  ggplot2::ggplot(data.frame(wd = wd, level = level), ggplot2::aes(x = wd)) +
    ggplot2::geom_histogram(binwidth = 0.025, fill = fig_pal$series[1],
      colour = fig_pal$surface, linewidth = 0.2) +
    ggplot2::facet_wrap(~level, ncol = 1, scales = "free_y",
      labeller = ggplot2::as_labeller(lab)) +
    ggplot2::labs(x = expression("Wood density (g cm"^-3*")"), y = "Stems") +
    themeFig()
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

#' Square extent of each plot, for maps of plots faceted with free scales
#'
#' Facets with free scales cannot use `coord_equal()`. Drawing these points
#'     with `geom_blank()`, with `theme(aspect.ratio = 1)`, gives each facet
#'     its own axes with equal x and y scales
#'
#' @param x dataframe with coordinates
#' @param x_col,y_col names of coordinate columns
#' @param group_col name of plot ID column
#'
#' @return dataframe with two rows per plot, the corners of a square centred
#'     on the plot, with sides equal to the larger of the x and y ranges
#'
squareExtent <- function(x, x_col, y_col, group_col) {
  out <- lapply(split(x, as.character(x[[group_col]])), function(g) {
    xr <- range(g[[x_col]], na.rm = TRUE)
    yr <- range(g[[y_col]], na.rm = TRUE)
    half <- max(diff(xr), diff(yr)) / 2
    data.frame(
      id = g[[group_col]][1],
      x = mean(xr) + c(-half, half),
      y = mean(yr) + c(-half, half))
  })
  out <- do.call(rbind, out)
  names(out) <- c(group_col, x_col, y_col)
  rownames(out) <- NULL
  out
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
