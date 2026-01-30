# Run scripts from their own working directory 
run_fn <- function(x) {
  stopifnot(file.exists(x))
  pwd <- getwd()
  setwd(dirname(normalizePath(x)))
  tryCatch(
    {
      source(basename(x))
    },
    error = function(e) { 
      message("Error while running '", x, "': ", e$message)
    }, 
    finally = {
      setwd(pwd)
    }
  )
}

#' Get valid UTM zone from latitude and longitude in WGS84 decimal degrees
#'
#' @param x vector of longitude coordinates in decimal degrees
#' @param y vector of latitude coordinate in decimal degrees
#'
#' @return Vector of UTM zones for each latitude-longitude pair
#' 
#' @export
#' 
latLong2UTM <- function(x, y) {
  unlist(lapply(1:length(x), function(z) {
    paste((floor((as.numeric(x[z]) + 180) / 6) %% 60) + 1,
      ifelse(as.numeric(y[z]) < 0, "S", "N"),
      sep = "")
  }))
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

#' Extract corner coordinates from sf polygons
#'
#' @param x sf object containing plot polygons, assumes all are rectangular
#' @param origin corner direction values either "SW", "NW", "NE", "SE". Either
#'     a single value for all plots, a vector with length equal to the number
#'     of rows in `x` with a single value per plot, or a list with length equal
#'     to the number of rows in `x` with a vector of values per plot. If NULL,
#'     or a vector or list element is NA, all corners are returned.
#' @param name optional column names in `x` to include in output
#' @param sf logical, if TRUE, an sf object is returned, otherwise a dataframe
#'
#' @return sf object containing corner points 
#' 
#' @importFrom dplyr bind_rows
#' @importFrom sf st_coordinates st_union st_sf st_sfc st_point st_crs
#' 
#' @export
#' 
polyCornerExtract <- function(x, corner = NULL, name = NULL, sf = TRUE) { 

  # Check arguments
  if (!is.null(name) && any(!name %in% colnames(x))) {
    stop("All values in 'name' must be columns in 'x'")
  }

  if (!is.null(corner) && !length(corner) %in% c(1, nrow(x))) {
    stop("length of 'corner' must be 1 or the number of rows in 'x'")
  }

  if (!is.null(corner) && !all(unlist(corner) %in% c("SW", "NW", "NE", "SE"))) {
    stop("all values in 'corner' must be 'SW', 'NW', 'NE', or 'SE'")
  }

  if (!isSFType(x, c("POLYGON", "MULTIPOLYGON"))) { 
    stop("'x' must be an sf object containing only POLYGON or MULTIPOLYGON")
  }

  # Repeat corner direction if necessary
  if (!is.null(corner) && !inherits(corner, "list")) {
    corner <- list(corner)
  }

  if (length(corner) == 1) {
    corner <- rep(corner, nrow(x))
   }

  # For each polygon
  out <- dplyr::bind_rows(lapply(seq_len(nrow(x)), function(i) {
    # Isolate polygon
    xsel <- x[i,]

    # Extract corner coordinates
    xc <- as.data.frame(sf::st_coordinates(sf::st_union(xsel)))

    if (!is.null(corner) && any(!is.na(corner[[i]]))) { 

      xc$sum <- xc$X + xc$Y
      xc$diff <- xc$X - xc$Y
      xc$corner_id <- NA_character_
      xc$corner_id[which.min(xc$diff)] <- "NW" 
      xc$corner_id[which.min(xc$sum)] <- "SW" 
      xc$corner_id[which.max(xc$diff)] <- "SE" 
      xc$corner_id[which.max(xc$sum)] <- "NE" 

      # Select chosen corner coordinate(s)
      xs <- xc[
        xc$corner_id %in% sort(corner[[i]]) & !is.na(xc$corner),
        c("X", "Y", "corner_id")]
    } else {
      xs <- xc
      xs$corner_id <- seq_len(nrow(xs))
    } 

    # Return selected corner coordinate(s)
    g <- sf::st_sfc(lapply(1:nrow(xs), function(j) {
        sf::st_point(as.matrix(xs[j,1:2]))
      }), crs = sf::st_crs(x))
    d <- sf::st_drop_geometry(x[rep(i, nrow(xs)), name])
    d$corner_id <- xs$corner_id
    sf::st_sf(d, geometry = g)
  }))

  if (!sf) { 
    out <- cbind(sf::st_drop_geometry(out), sf::st_coordinates(out))
  }

  # Return
  return(out)
}

#' Correct and match taxonomic names to the World Flora Taxonomic Backbone
#'
#' @param x vector of taxonomic names
#' @param WFO.file optional file name of static copy of World Flora Online
#'     Taxonomic Backbone. If not NULL, data will be reloaded from this file
#' @param WFO.data optional dataset with static copy of World Flora Online
#'     Taxonomic backbone. Ignored if `WFO.file` is not NULL
#' @param lookup optional a single dataframe or a list of dataframes containing
#'     lookup tables. The first column should contain names in `x` to be
#'     changed. The second column should contain the new names.
#' @param ret_wfo logical, if TRUE the function stops after
#'     `WorldFlora::WFO.match()` and returns the raw output from this function.
#' @param ret_unk logical, if TRUE taxa not matched in the World
#'     Flora Online are returned to the user as a vector containing
#'     the unmatched values. If FALSE these taxa are returned as NA.
#' @param ret_multi logical, if TRUE taxa matching multiple records in the
#'     World Flora Online are returned to the user as a list with one element
#'     for each original name containing the unmatched values. If FALSE the
#'     "best" name is selected by `WorldFlora::WFO.one()`
#' @param sub.pattern vector with regular expressions defining sections of `x`
#'     to be removed during correction of common orthographic errors by
#'     `WorldFlora::WFO.prepare()`
#' @param fuzzy If larger than 0, then attempt fuzzy matching. See `WorldFlora::WFO.match()`
#' @param ... Additional arguments passed to `WorldFlora::WFO.match()`
#'
#' @return Dataframe with cleaned taxonomic names and metadata
#'
#' @details
#' Taxonomic names are matched against the World Flora Online database using
#' `WorldFlora::WFO.match()`.
#' 
#' The search algorithm is as follows:
#'     \enumerate{
#'       \item{Optionally replace names with `lookup`}
#'       \item{Correct common orthographic errors with `WorldFlora::WFO.prepare()`}
#'       \item{Query `WorldFlora::WFO.match()` for accepted
#'             name and taxonomic rank information}
#'       \item{Optionally return multiple matches or unsuccessful matches}
#'       \item{Consolidate multiple matches with `WorldFlora::WFO.one()`}
#'       \item{Return formatted dataframe}
#'     }
#' 
#' Names that cannot be matched should be replaced with "Indet indet" in
#' `lookup`. These are replaced with NA_character_ before `WorldFlora::WFO.match()`
#'
#' @importFrom data.table fread data.table
#' @importFrom WorldFlora WFO.prepare WFO.match WFO.one
#'
#' @export
#'
taxonCheck <- function(x, WFO.file = NULL, WFO.data = NULL, 
   lookup = NULL, ret_wfo = FALSE, ret_unk = FALSE, ret_multi = FALSE,
   sub.pattern = WFO.prepare_default(), fuzzy = 0.1, ...) {

  # Check WFO data is available
  if (is.null(WFO.data) & is.null(WFO.file)) {
    stop("Either WFO.data or WFO.file must be provided")
  }

  if (is.null(WFO.data)) {
    message(paste("Reading WFO data"))
    if (!file.exists(WFO.file)) {
      stop("If WFO.data is NULL, a valid WFO.file must be provided. See WorldFlora::WFO.download()")
    }
    WFO.data <- data.table::fread(WFO.file, encoding = "UTF-8")
  } else {
    WFO.data <- data.table::data.table(WFO.data)
  }

  WFO.data$scientificName <- gsub("\\s+", " ", WFO.data$scientificName)

  # Get unique taxonomic names
  xu <- unique(x)

  # Substitute names with lookup table
  if (!is.null(lookup)) {
    message("Substituting names with `lookup`")
    xf <- synonymyFix(xu, lookup = lookup)
  } else {
    xf <- xu
  }

  # Prepare taxonomic names for WFO query
  xs <- WorldFlora::WFO.prepare(xf, sub.pattern = sub.pattern)$spec.name
  
  # Replace Indet genera with ""
  xi <- xs
  xi[xi == "Indet"] <- ""

  # Run WFO matching 
  message("Querying World Flora Online")
  wfo <- WorldFlora::WFO.match(unique(xi), 
    WFO.data = WFO.data, Fuzzy = fuzzy)


  # Optionally return raw WFO output  
  if (ret_wfo) { 
    # Add original names
    wfo_all <- dplyr::bind_rows(lapply(seq_along(xu), function(i) {
      orig <- xu[i]
      cbind("taxon_name_orig" = orig, wfo[wfo$spec.name.ORIG == xi[i],])
    }))

    # Check all original names are matched back 
    stopifnot(all(!is.na(wfo_all$taxon_name_orig)))

    return(wfo_all)
  }

  # Consolidate to single best name per taxon
  wfo_one <- WorldFlora::WFO.one(wfo, verbose = FALSE)

  # Add original names
  wfo_one_all <- dplyr::bind_rows(lapply(seq_along(xu), function(i) {
    orig <- xu[i]
    cbind("taxon_name_orig" = orig, wfo_one[wfo_one$spec.name.ORIG == xi[i],])
  }))

  # Check all original names are matched back 
  stopifnot(all(!is.na(wfo_one_all$taxon_name_orig)))
  
  wfo_sel <- wfo_one_all[,c(
    "taxon_name_orig",
    "spec.name.ORIG",  # taxon_name_sanit
    "Old.name",  # taxon_name_syn
    "Old.ID",  # taxon_wfo_syn
    "scientificName",  # taxon_name_acc
    "taxonID",  # taxon_wfo_acc
    "scientificNameAuthorship",  # taxon_auth_acc
    "taxonRank",  # taxon_rank_acc
    "parentNameUsageID",  # taxon_wfo_parent
    "specificEpithet",  # taxon_epithet_acc
    "genus",  # taxon_genus_acc
    "family"  # taxon_family_acc
  )]
  wfo_sel <- unique(wfo_sel)

  # All submitted names should be included in WFO output
  stopifnot(all(sort(unique(wfo_sel$spec.name.ORIG)) == sort(unique(xi))))

  # Consolidate genus and species
  wfo_sel$species <- trimws(paste(wfo_sel$genus, wfo_sel$specificEpithet))

  wfo_sel$species <- ifelse(!wfo_sel$taxonRank %in% 
      c("species", "subspecies", "variety", "subvariety", 
          "form", "subform", "prole", "unranked"), 
    NA_character_, wfo_sel$species)

  # Extract subsp. and var. epithets from accepted names
  wfo_sel$taxon_subspecies_acc <- gsub(".*subsp\\.\\s", "", wfo_sel$scientificName)
  wfo_sel$taxon_subspecies_acc[!grepl("\\ssubsp\\.\\s", wfo_sel$scientificName)] <- NA_character_

  wfo_sel$taxon_variety_acc <- gsub(".*var\\.\\s", "", wfo_sel$scientificName)
  wfo_sel$taxon_variety_acc[!grepl("\\svar\\.\\s", wfo_sel$scientificName)] <- NA_character_

  # Fill wfo ID of synonyms
  wfo_sel$Old.ID <- ifelse(wfo_sel$Old.ID == "", 
    wfo_sel$taxonID, wfo_sel$Old.ID)

  wfo_sel$Old.name <- ifelse(wfo_sel$Old.name == "", 
    wfo_sel$scientificName, wfo_sel$Old.name)

  # Add date of processing
  wfo_sel$taxon_wfo_date <- Sys.Date()

  # Create output dataframe
  out <- wfo_sel[,c(
    "taxon_name_orig",  
    "spec.name.ORIG",  # taxon_name_sanit
    "Old.name",  # taxon_name_syn
    "Old.ID",  # taxon_wfo_syn
    "scientificName",  # taxon_name_acc
    "taxonID",  # taxon_wfo_acc
    "scientificNameAuthorship",  # taxon_auth_acc
    "taxonRank",  # taxon_rank_acc
    "parentNameUsageID",  # taxon_wfo_parent
    "taxon_variety_acc",
    "taxon_subspecies_acc",
    "specificEpithet",  # taxon_epithet_acc
    "species",  # taxon_species_acc
    "genus",  # taxon_genus_acc
    "family",  # taxon_family_acc
    "taxon_wfo_date")]

  names(out) <- c(
    "taxon_name_orig",
    "taxon_name_sanit",
    "taxon_name_syn",
    "taxon_wfo_syn",
    "taxon_name_acc",
    "taxon_wfo_acc",
    "taxon_auth_acc",
    "taxon_rank_acc",
    "taxon_wfo_parent",
    "taxon_variety_acc",
    "taxon_subspecies_acc",
    "taxon_epithet_acc",
    "taxon_species_acc",
    "taxon_genus_acc",
    "taxon_family_acc",
    "taxon_wfo_date")

  # Optionally return unmatched names
  if (ret_unk & any(out$taxon_name_sanit != out$taxon_name_syn, na.rm = TRUE)) {
    unmatched <- out$taxon_name_orig[
      (out$taxon_name_sanit != out$taxon_name_syn) | 
        is.na(out$taxon_name_syn) | is.na(out$taxon_name_sanit)]
    warning("Some taxonomic names not matched by WFO.match, returning original names")
    return(unmatched)
  }

  # Optionally return names with multiple matches
  if (ret_multi & any(duplicated(wfo$taxon_name_orig))) {
    multis <- wfo$taxon_name_orig[duplicated(wfo$taxon_name_orig)]
    multis_df <- wfo[wfo$taxon_name_orig %in% multis,]
    multis_list <- split(multis_df, multis_df$taxon_name_orig)
    warning("Some taxonomic names matched to multiple names by WFO.match, returning options")
    return(multis_list)
  }

  # Change "" to NA in all columns
  out[] <- lapply(out, function(x) {
    if (is.character(x)) {
      x[x == ""] <- NA_character_
    }
    x
  })

  # All original names should be filled
  stopifnot(all(!is.na(out$species[out$species_sanit != "Indet indet"])))

  # Return
  return(out)
}

#' Replace taxonomic names using lookup tables
#'
#' @param x vector of species names
#' @param lookup a single dataframe or a list of dataframes containing lookup
#'     tables. The first column should contain names in `x` to be changed. The
#'     second column should contain the new names.
#'
#' @return Vector of corrected species names
#' 
#' @details Lookup tables are run in order through the list of lookup tables, meaning 
#' names may change incrementally multiple times.
#' 
#' @importFrom dplyr bind_rows
#' 
#' @export
#' 
synonymyFix <- function(x, lookup) {

  # Make list if not already
  if (!inherits(lookup, "list")) {
    lookup <- list(lookup)
  }

  # Combine lookup tables into a single dataframe
  lookup_combi <- as.data.frame(dplyr::bind_rows(lookup))

  # Check no NAs
  if (any(is.na(lookup_combi))) {
    stop("Lookup table cannot contain NA entries")
  }

  # Do substitution
  out <- lookup_combi[,2][match(x, lookup_combi[,1])]
  out[is.na(out)] <- x[is.na(out)]

  return(out)
}

#' Return default pattern substitution for `taxonCheck()`
#'
#' @return vector of regex patterns for use with `taxonCheck()` in argument
#'     `sub.pattern`
#' 
#' @export
#' 
WFO.prepare_default <- function() { 
  c(
    " indet$",
    " sp[.]",
    " spp[.]",
    " ssp[.]",
    " pl[.]",
    " indet[.]",
    " ind[.]",
    " gen[.]",
    " g[.]",
    " fam[.]",
    " nov[.]",
    " prox[.]",
    " cf[.]",
    " aff[.]",
    " s[.]s[.]",
    " s[.]l[.]",
    " p[.]p[.]",
    " p[.] p[.]",
    "[?]",
    " inc[.]",
    " stet[.]",
    "nom[.] cons[.]",
    "nom[.] dub[.]",
    " nom[.] err[.]",
    " nom[.] illeg[.]",
    " nom[.] inval[.]",
    " nom[.] nov[.]",
    " nom[.] nud[.]",
    " nom[.] obl[.]",
    " nom[.] prot[.]",
    " nom[.] rej[.]",
    " nom[.] supp[.]",
    " sensu auct[.]"
  )
}

divide_plot2 <- function(corner_data, rel_coord, proj_coord = NULL, longlat = NULL, grid_size, tree_data = NULL, tree_coords = NULL, corner_plot_ID = NULL, tree_plot_ID = NULL, grid_tol = 0.1, origin = "bottomleft") {
  
  # Checking arguments ---------------------------------------------------------
  
  if(missing(corner_data)) {
    stop("The way in which arguments are provided to the function has changed since version 2.2.1. You now have to provide corner_data data frame and its associated coordinates variable names.")
  }
  if(!is.data.frame(corner_data)){
    stop("corner_data must a data frame or a data frame extension")
  }
  if (!any(rel_coord %in% names(corner_data))) {
    stop("column names provided by rel_coord are not found in corner_data")
  }
  if (!is.null(proj_coord) && !any(proj_coord %in% names(corner_data))) {
    stop("column names provided by proj_coord are not found in corner_data")
  }
  if (!is.null(longlat) && !any(longlat %in% names(corner_data))) {
    stop("column names provided by longlat are not found in corner_data")
  }
  if(!length(grid_size) %in% c(1,2)) {
    stop("The length of grid_size must be equal to 1 or 2\nIf you want to divide several plots with different grid sizes, you must apply yourself the function for each plot")
  }
  if(nrow(corner_data)!=4 & is.null(corner_plot_ID)){
    stop("You must provide corner_plot_ID if you have more than one plot in your data")
  }
  if (!is.null(corner_plot_ID) && !any(corner_plot_ID==names(corner_data))) {
    stop(paste(corner_plot_ID,"is not found in corner_data column names."))
  }
  if(!is.null(corner_plot_ID) && !all(sapply(split(corner_data,corner_data[[corner_plot_ID]]) , nrow) == 4)){
    stop("corner_data does'nt contain exactly 4 corners by plot")
  }
  if(!is.null(tree_data) && !is.data.frame(tree_data)){
    stop("tree_data must be a data frame or a data frame extension")
  }
  if(!is.null(tree_data) && is.null(tree_coords)) {
    stop("You must provide the column names of the relative coordinates of the trees using the tree_coords argument")
  }
  if(!is.null(tree_data) && !any(tree_coords %in% names(tree_data))) {
    stop("column names provided by tree_coords are not found in tree_data colunm names")
  }
  if(nrow(corner_data)!=4 & !is.null(tree_data) & is.null(tree_plot_ID)){
    stop("You must provide tree_plot_ID if you have more than one plot in your data")
  }
  if (!is.null(tree_plot_ID) && !any(tree_plot_ID==names(tree_data))) {
    stop(paste(tree_plot_ID,"is not found in tree_data column names."))
  }
  if (!origin %in% c("bottomleft", "bottomright", "topright", "topleft", "centre")) { 
    stop("origin must be one of: 'bottomleft', 'bottomright', 'topright', 'topleft', 'centre'")
  }
  
  # Data processing ------------------------------------------------------------
  
  if(length(grid_size)!=2) grid_size = rep(grid_size,2)
  
  corner_dt <- data.table(corner_data)
  
  setnames(corner_dt, old = rel_coord, new = c("x_rel","y_rel"))

  if(!is.null(proj_coord)) {
    setnames(corner_dt, old = proj_coord, new = c("x_proj","y_proj"))
  }
  if(!is.null(longlat)) {
    setnames(corner_dt, old = longlat, new = c("long","lat"))
  }
  
  if(!is.null(corner_plot_ID)) {
    setnames(corner_dt, old = corner_plot_ID, new = "corner_plot_ID")
  } else {
    corner_dt[, corner_plot_ID := "" ]
  }

  # Sorting rows in a counter-clockwise direction and check for non-rectangular plot
  sort_rows <- function(dat) { # dat = corner_dt
    centroid <- colMeans(dat[,c("x_rel","y_rel")])
    angles <- atan2(dat[["y_rel"]] - centroid[2], dat[["x_rel"]] - centroid[1])
    dat <- dat[order(angles), ]
    # check for non-rectangular plot : distances between centroid and corners must be equals
    if(!all(abs(dist(rbind(dat[,c("x_rel","y_rel")],as.data.frame.list(centroid)))[c(4,7,9,10)] - mean(dist(rbind(dat[,c("x_rel","y_rel")],as.data.frame.list(centroid)))[c(4,7,9,10)]))<0.1)) {
      stop("The plot in the relative coordinate system is not a rectangle (or a square). BIOMASS package can't deal with non-rectangular plot")
    }
    return(dat)
  }
  
  corner_dt <- corner_dt[, sort_rows(.SD), by = corner_plot_ID]

  
  # Transform the geographic coordinates into UTM coordinates ------------------
  
  latlong2UTM_fct <- function(dat) { # dat = corner_dt
    proj_coord <- latlong2UTM(dat[, c("long","lat")])
    UTM_code <- unique(proj_coord[, "codeUTM"])
    if(length(UTM_code)>1) {
      stop(paste(unique(dat$plot_ID), "More than one UTM zone are detected. This may be due to an error in the long/lat coordinates, or if the parcel is located right between two UTM zones. In this case, please convert yourself your long/lat coordinates into any projected coordinates which have the same dimension than your local coordinates"))
    }
    corner_dt[corner_plot_ID %in% dat$corner_plot_ID, c("x_proj", "y_proj") := list(x_proj = proj_coord$X, y_proj = proj_coord$Y)]
    return(data.frame(UTM_code = UTM_code))
  }
  # Apply latlong2UTM_fct to all plots if necessary
  if(!is.null(longlat)) {
    UTM_code <- corner_dt[, latlong2UTM_fct(.SD), by = corner_plot_ID, .SDcols = colnames(corner_dt)]
  }
  
  
  # Dividing plots   -----------------------------------------------------------
  
  # Grids the plot from the relative coordinates and calculates the projected coordinates of the grid points.
  divide_plot_fct <- function(dat, grid_size) { # dat = corner_dt
    
    # Check that grid dimensions match plot dimensions
    x_plot_length <- diff(range(dat[["x_rel"]]))
    y_plot_length <- diff(range(dat[["y_rel"]]))
    x_not_in_grid <- x_plot_length %% grid_size[1]
    y_not_in_grid <- y_plot_length %% grid_size[2]
    if( x_not_in_grid != 0 ) warning("\nThe x-dimension of the plot is not a multiple of the x-dimension of the grid size")
    if( y_not_in_grid != 0 ) warning("\nThe y-dimension of the plot is not a multiple of the y-dimension of the grid size")
    if( x_not_in_grid * y_plot_length + y_not_in_grid * x_plot_length - x_not_in_grid * y_not_in_grid > grid_tol * x_plot_length * y_plot_length ) {
      stop(paste("More than",grid_tol*100,"% of the plot area is not included in the sub-plot grid. If you still want to divide the plot, please increase the value of the grid_tol argument."))
    }
    # Create grid coordinates
    xmin <- min(dat[["x_rel"]])
    xmax <- max(dat[["x_rel"]])
    ymin <- min(dat[["y_rel"]])
    ymax <- max(dat[["y_rel"]])

    width  <- xmax - xmin
    height <- ymax - ymin

    xoff <- 0
    if (origin %in% c("bottomright", "topright")) {
      xoff <- width %% grid_size[1]
    }

    yoff <- 0
    if (origin %in% c("topleft", "topright")) {
      yoff <- height %% grid_size[2]
    }
    if (origin == "centre") {
      xoff <- (width/2) %% grid_size[1]
      yoff <- (height/2) %% grid_size[2]
    }
      
    xseq <- seq(xmin + xoff, xmax, by = grid_size[1])
    yseq <- seq(ymin + yoff, ymax, by = grid_size[2])
      
    # Create grid intersection points
    plot_grid <- data.table(expand.grid(x_rel = xseq, y_rel = yseq))
    plot_grid[,corner_plot_ID:=unique(dat$corner_plot_ID)]
    
    # Attributing subplots names to each corner and adding shared subplot corners
    plot_grid <- rbindlist(apply(plot_grid[x_rel < max(x_rel) & y_rel < max(y_rel),], 1, function(grid_dat) {
      X <- as.numeric(grid_dat[["x_rel"]])
      Y <- as.numeric(grid_dat[["y_rel"]])
      plot_grid[
        (x_rel == X & y_rel == Y) | (x_rel == X + grid_size[1] & y_rel == Y) | (x_rel == X + grid_size[1] & y_rel == Y + grid_size[2]) | (x_rel == X & y_rel == Y + grid_size[2]),
        .(subplot_ID = paste0(
          corner_plot_ID, ":", 
          (X-min(plot_grid$x_rel)) / grid_size[1], "_",
          (Y-min(plot_grid$y_rel)) / grid_size[2]), x_rel, y_rel)]
    }))
    
    # Sorting rows 
    plot_grid <- plot_grid[, sort_rows(.SD), by=subplot_ID]
    
    # Transformation of relative grid coordinates into projected coordinates if provided
    if(!is.null(proj_coord) | !is.null(longlat)) {
      plot_grid <- cbind(plot_grid,bilinear_interpolation(coord = plot_grid[,c("x_rel","y_rel")] , from_corner_coord = dat[,c("x_rel","y_rel")] , to_corner_coord = dat[,c("x_proj","y_proj")], ordered_corner = T))
    }
    return(plot_grid)
  }
  
  # Apply divide_plot_fct to all plots
  sub_corner_coord <- corner_dt[, divide_plot_fct(.SD, grid_size), by = corner_plot_ID, .SDcols = colnames(corner_dt)]
  
  
  # Retrieving geographic coordinates ------------------------------------------
  if(!is.null(longlat)) {
    GPS_coord_fct <- function(dat) { # dat = sub_corner_coord
      gps_coord <- as.data.frame( proj4::project(dat[,c("x_proj","y_proj")], proj = UTM_code$UTM_code[UTM_code$corner_plot_ID == unique(dat$corner_plot_ID)], inverse = TRUE) )
      sub_corner_coord[corner_plot_ID %in% dat$corner_plot_ID, c("long", "lat") := list(long = gps_coord$x, lat = gps_coord$y)]
    }
    sub_corner_coord[, GPS_coord_fct(.SD), by = corner_plot_ID, .SDcols = colnames(sub_corner_coord)]
  }
  
  
  
  # Assigning trees to subplots ------------------------------------------------
  
  if(!is.null(tree_data)) {
    
    tree_dt <- data.table(tree_data)
    
    setnames(tree_dt, old = tree_coords, new = c("x_rel","y_rel"))
    
    if(!is.null(tree_plot_ID)) {
      setnames(tree_dt, old = tree_plot_ID, new = "plot_ID")
      
      if(any(! unique(tree_dt[["plot_ID"]]) %in% unique(corner_dt[["corner_plot_ID"]]))) {
        warning( paste( "These ID's are found in tree_plot_ID but not in corner_data :" , paste(unique(tree_dt[["plot_ID"]])[! unique(tree_dt[["plot_ID"]]) %in% unique(corner_dt[["corner_plot_ID"]])] , collapse = " "),"\n") )
      }
      
    } else {
      tree_dt[, plot_ID := "" ]
    } 
    
    invisible(lapply(split(sub_corner_coord, by = "subplot_ID", keep.by = TRUE), function(dat) {
      tree_dt[ plot_ID == dat$corner_plot_ID[1] &
                 x_rel %between% range(dat[["x_rel"]]) &
                 y_rel %between% range(dat[["y_rel"]]),
               subplot_ID := dat$subplot_ID[1]]
    }))
    
    if (anyNA(tree_dt[, subplot_ID])) {
      warning("One or more trees could not be assigned to a subplot (not in a subplot area)")
    }

    if(is.null(tree_plot_ID)) {
      tree_dt[ !is.na(subplot_ID) , subplot_ID := paste0("subplot",subplot_ID) ]
      tree_dt[ , plot_ID := NULL ]
    } 
  }
  
  # Returns --------------------------------------------------------------------
  
  if(is.null(corner_plot_ID)) {
    sub_corner_coord[ , c("subplot_ID","corner_plot_ID") := list(paste0("subplot",subplot_ID),NULL)]
  }
  
  if(is.null(tree_data)) { # tree_data absent
    if (is.null(longlat)) { # geographic coordinates absent
      output <- data.frame(sub_corner_coord)
    } else { # geographic coordinates present
      if(all(UTM_code[,corner_plot_ID]=="")) { # single plot
        UTM_code$corner_plot_ID <- NULL
      }
      output <- list(sub_corner_coord = data.frame(sub_corner_coord), 
                     UTM_code = UTM_code)  
    }
  } else { # tree_data present
    if (is.null(longlat)) { # geographic coordinates absent
      output <- list(sub_corner_coord = data.frame(sub_corner_coord), 
                     tree_data = data.frame(tree_dt))
    } else { # geographic coordinates present
      if(all(UTM_code[,corner_plot_ID]=="")) { # single plot
        UTM_code$corner_plot_ID <- NULL 
      }
      output <- list(sub_corner_coord = data.frame(sub_corner_coord), 
                     tree_data = data.frame(tree_dt),
                     UTM_code = UTM_code)  
    }
    
  }
  
  return(output)
}
