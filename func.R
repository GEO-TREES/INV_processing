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
    bad_cols <- colnames(x)[!col_class_test]
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

#' Check stem table values
#' 
#' Runs various checks on the values in stem measurement table columns 
#'
#' @param x dataframe containing stem measurements
#'
stemValCheck <- function(x) {
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

  # Measurement ID must be positive integer
  if (any(x$measurement_id <= 0 | x$measurement_id != floor(x$measurement_id), na.rm = TRUE)) { 
    stop("`measurement_id` must be a positive integer")
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

  # Codes must only contain some values
  code_allowed <- c("A", "D", "S", "F", "M", "B", "T") # Letters and spaces
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
}

#' Check taxon table values
#'
#' Runs various checks on the values in taxon table columns 
#'
#' @param x dataframe containing taxonomic data 
#' 
taxonValCheck <- function(x) { 
  # Wood density must be positive
  if (any(x$wood_density_gcm3 <= 0, na.rm = TRUE)) { 
    stop("`wood_density_gcm3` must be positive")
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
#'
plotValCheck <- function(x) {
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

  # All censuses must have a census date
  if (any(is.na(x$census_date))) { 
    stop("NAs in `census_date` are not allowed")
  }

  # Census date must be either YYYY, YYYY-MM, YYYY-MM-DD
  if (any(!grepl("^\\d{4}(-\\d{2}){0,2}$", x$census_date))) {
    stop("`census_date` must be formatted either YYYY, YYYY-MM, or YYYY-MM-DD")
  }

  # Plot length must be positive
  if (!all(!is.na(x$plot_length_m) & x$plot_length_m > 0)) {
    stop("`plot_length_m` must be positive")
  }

  # Plot width must be positive
  if (!all(!is.na(x$plot_width_m) & x$plot_width_m > 0)) {
    stop("`plot_width_m` must be positive")
  }

  # Plot planar must be provided
  if (any(is.na(x$plot_planar))) { 
    stop("`plot_planar` must be provided")
  }

  # Minimum diameter threshold must be positive
  if (!all(!is.na(x$meas_diam_min_cm) & x$meas_diam_min_cm > 0)) {
    stop("`meas_diam_min_cm` must be positive")
  }

  # Default POM must be positive
  if (!all(!is.na(x$meas_pom_default_m) & x$meas_pom_default_m > 0)) {
    stop("`meas_pom_default_m` must be positive")
  }

  # Methods logical columns must be provided
  if (any(is.na(x$meas_tree_stem))) {
    stop("`meas_tree_stem` must be provided")
  }

  if (any(is.na(x$meas_tree_group))) {
    stop("`meas_tree_group` must be provided")
  }

  if (any(is.na(x$meas_dead))) {
    stop("`meas_dead` must be provided")
  }

  if (any(is.na(x$meas_fallen))) {
    stop("`meas_fallen` must be provided")
  }

}

#' Check plot corner sf object values
#' 
#' Runs various checks on the values in plot corner sf object columns 
#'
#' @param x sf dataframe containing plot corner points
#'
ptValCheck <- function(x) {
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

  # Corner IDs must be unique within a plot
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

  # Must be SF type
  if (!isSFType(x, "POINT")) { 
    stop("Must be sf POINT object")
  }

  # Must be WGS84 CRS
  if (sf::st_crs(x) != st_crs(4326)) {
    stop("Must be WGS84 EPSG:4326")
  }
}

#' Check values across data objects
#' 
#' Runs various checks on values across plot, stem, pt and taxon objects
#'
#' @param plot 
#' @param stem 
#' @param pt 
#' @param taxon 
#'
valCheck <- function(plot = NULL, stem = NULL, pt = NULL, taxon = NULL) { 

  # Run single table checks
  if (!is.null(plot)) {
    plotValCheck(plot)
  }

  if (!is.null(stem)) {
    stemValCheck(stem)
  }

  if (!is.null(pt)) {
    ptValCheck(pt)
  }

  if (!is.null(taxon)) {
    taxonValCheck(taxon)
  }

  # Run tests to match plot and stem tables
  if (!is.null(plot) & !is.null(stem)) {
    # All plots and censuses in stem must be in plot
    if (!all(paste(stem$plot_id, stem$census_date) %in% paste(plot$plot_id, plot$census_date))) {
      stop("Some plot-census combinations in 'stem' are missing from 'plot'")
    }

    # Acquisition IDs must match
    if (any(unique(plot$acquisition_id) != unique(stem$acquisition_id))) {
      stop("Acquisition IDs do not match between 'plot' and 'stem'")
    }
  }

  # Run tests to match plot and pt tables
  if (!is.null(plot) & !is.null(pt)) {
    # All plots must match between plot and pt
    if (any(sort(unique(pt$plot_id)) != sort(unique(plot$plot_id)))) {
      stop("'plot_id' does not match between 'plot' and 'pt'")
    }
  }

  # Run tests to match stem and taxon tables
  if (!is.null(stem) & !is.null(taxon)) {
    # TODO:
  }
}

#' Import saved WFO cache file
#'
#' Loads the cache into namespace BIOMASS:::the$wfo_cache
#'
#' @param filepath to previously created WFO cache file
#'
loadWFOCache <- function(x) {
  wfo_cache <- readRDS(x)

  # Get internal namespace environment of the package
  pkg_env <- asNamespace("BIOMASS")

  # Overwrite object 'the' within namespace env
  the_modified <- BIOMASS:::the
  the_modified$wfo_cache <- wfo_cache

  # Force it back into the namespace env
  unlockBinding("the", pkg_env) 
  assign("the", the_modified, envir = pkg_env)
  lockBinding("the", pkg_env) 
}

# Check if object is sf type
#'
#' @param x object subject to test
#' @param type optional character vector of acceptable sf geometry types
#'
#' @return logical
#'
#' @importFrom sf st_geometry_type
#' 
#' @export
#' 
isSFType <- function(x, type = NULL) {
  inherits(x, c("sf", "sfc")) && (is.null(type) |
    all(sf::st_geometry_type(x, by_geometry = FALSE) %in% type))
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
  ret <-
    apply(
      X = cbind(...),
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
  if (!is.null(collapse)) {
    paste(ret, collapse = collapse)
  } else {
    ret
  }
}

#' Identify discrete censuses from a vector of measurement dates
#'
#' @param x vector of measurement dates, character or Date
#' @param gap number of days above which consecutive measurement dates will be
#'     split into different censuses
#'
#' @return character vector of census mid-dates (median) for each value in `x`
#' 
censusGen <- function(x, gap) {
  # Store original order and create a working data frame
  orig_order <- seq_along(x)
  dat <- data.frame(m_date = as.Date(x), id = orig_order)
  
  # Sort by date to identify chronological gaps
  dat <- dat[order(dat$m_date), ]
  
  # Calculate gaps and assign census IDs
  # diff() on Date returns days
  # Prepend 0 to keep length consistent
  gaps <- c(0, diff(dat$m_date))
  dat$census_id <- cumsum(gaps > gap)
  
  # Calculate mid-date (median) per census
  # Convert to numeric for ave(), then back to Date
  dat$census_date <- as.character(as.Date(
    ave(as.numeric(dat$m_date), dat$census_id, FUN = median)))
  
  # Restore original order 
  out <- dat[order(dat$id), "census_date"]
  
  # Return
  return(out)
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
getUTM <- function(lon, lat, epsg = TRUE) {
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

