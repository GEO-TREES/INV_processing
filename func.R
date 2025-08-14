# Define function to run scripts from own working directory 
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

#' Calculate the bearings between pairs of points
#'
#' @param x sf points containing origin points
#' @param y sf points containing end points, in the same row order as `x`
#' @param name optional column names in `x` and `y` to include in output 
#' @param deg if TRUE angle returned in degrees rather than radians
#'
#' @return numeric vector or dataframe (if name provided) with angles of bearing
#' 
#' @export
#' 
bearing <- function(x, y, name = NULL, deg = FALSE) {

  # Checks 
  if (!is.null(name) && any(!name %in% colnames(x))) {
    stop("All values in 'name' must be columns in 'x' and 'y'")
  }

  if (!isSFType(x, c("POINT")) | 
        !isSFType(y, c("POINT"))) { 
    stop("'x' and 'y' must be sf objects containing only POINT")
  }

  # Define function to do the calculation
  bearing_fn <- function(x, y) {
    theta <- atan2(y[2] - x[2], y[1] - x[1])
    if (deg) { theta <- theta * 180 / pi }
    return(theta)
  }

  # Optionally order x and y by names
  if (!is.null(name)) { 
    xkey <- apply(st_drop_geometry(x[,name]), 1, paste, collapse = "::")
    ykey <- apply(st_drop_geometry(y[,name]), 1, paste, collapse = "::")
    yord <- y[match(xkey, ykey),]
    if (any(is.na(yord[,name]))) {
      stop("Values in 'name' do not match across 'x' and 'y'")
    }
  }

  # For each starting point
  out <- unlist(lapply(seq_len(nrow(x)), function(i) {
    unname(bearing_fn(sf::st_coordinates(x)[i,], sf::st_coordinates(yord)[i,]))
  }))

  # Add names
  if (!is.null(name)) {
    out <- st_drop_geometry(cbind(x[,name], angle = out))
  }

  # Return
  return(out)
}

#' Perform rotation on geometry objects
#'
#' @param x either an sf object, a vector of length two with X and Y 
#'     coordinates, or a matrix with two columns.
#' @param origin either an sf object with origins of rotation, a vector of
#'     length two with X and Y coordinates, or a matrix with two columns.
#' @param angle vector of angle values, in radians
#' @param sf logical, if TRUE, an sf object is returned, otherwise a dataframe
#' 
#' @return
#'
#' @importFrom sf st_geometry st_coordinates st_geometry_type st_polygon st_multipolygon st_linestring st_multilinestring st_point st_multipoint st_sf st_sfc st_crs
#'
#' @export
#' 
rotation <- function(x, origin, angle, sf = TRUE) { 

  # Check arguments
  if (!length(angle) %in% c(1, nrow(x))) {
    stop("length of 'angle' must be 1 or the number of rows in 'x'")
  }

  if (!all(is.numeric(angle))) {
    stop("all values in 'angle' must be numeric")
  }

  if (isSFType(origin) && !isSFType(origin, "POINT")) {
    stop("'origin' sf objects must contain only POINT")
  }

  if (isSFType(origin) && !length(sf::st_geometry(origin)) %in% c(1, nrow(x))) {
    stop("number of geometries in 'origin' must be 1 or the number of rows in 'x'")
  }

  if (!isSFType(origin) && !nrow(origin) %in% c(1, nrow(x))) {
    stop("number of geometries in 'origin' must be 1 or the number of rows in 'x'")
  }

  # Convert origins to matrix if necessary
  if (isSFType(origin)) {
    origin <- st_coordinates(origin)[,1:2]
  }

  # Repeat origins of rotation if necessary
  if (!isSFType(origin) & !inherits(origin, "matrix")) { 
    origin <- matrix(rep(origin, nrow(x)), ncol = 2, byrow = TRUE)
  }

  # Repeat angles of rotation if necessary
  if (length(angle) == 1) { 
    angle <- rep(angle, nrow(x))
  }

  # Convert x to list of matrices if necessary
  if (!isSFType(x) & !inherits(x, "matrix")) {
    xm <- list(matrix(x, ncol = 2, byrow = TRUE))
  }

  if (isSFType(x)) {
    xm <- lapply(seq_len(nrow(x)), function(i) { sf::st_coordinates(x[i,]) })
  }

  # Define helper functions to do rotation
  rot <- function(a) { 
    matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2) 
  }

  rot_fn <- function(g, o, a) {
    om <- matrix(o, nrow(g), 2, byrow = TRUE)
    ((g - om) %*% rot(a)) + om
  }

  # For each geometry
  rg <- lapply(seq_along(xm), function(i) {
    # Rotate coordinates
    rot_fn(xm[[i]][,1:2], origin[i,], angle[i])
  })

  # Optionally construct sf object
  if (sf == TRUE) {
    rg <- lapply(seq_along(rg), function(i) {
      switch(as.character(sf::st_geometry_type(x[i,])),
        POLYGON = sf::st_polygon(rg[i]),
        LINESTRING = sf::st_linestring(rg[[i]]),
        POINT = sf::st_point(rg[[i]]),
        stop("Unsupported geometry type")
      )
    })

    # Construct output sf object
    out <- sf::st_sf(st_drop_geometry(x), geometry = sf::st_sfc(rg, crs = sf::st_crs(x)))
  } else {
    out <- dplyr::bind_rows(lapply(seq_along(rg), function(i) {
      cds <- as.data.frame(rg[[i]])
      names(cds) <- c("X", "Y")
      cbind(st_drop_geometry(x[i,]), cds)
    }))
  }

  # Return
  return(out)
}

#' A fast alternative to `rotation()`
#'
#' @importFrom sf st_coordinates
#' 
#' @export
#' 
rotation2 <- function(x, origin, angle) { 
  # Extract coordinates 
  xc <- sf::st_coordinates(x)
  oc <- sf::st_coordinates(origin)

  # Subtract origin
  xo <- sweep(xc[,1:2], 2, oc, `-`)

  # Rotate 
  xr <- xo %*% rot(angle) 

  # Add origin 
  xro <- sweep(xr, 2, oc, `+`)

  # Return
  return(xro)
}

#' Split a plot into regular subplots
#'
#' @param x sf object containing plot corners as point geometries, or a list of
#'     dataframes containing corner coordinates
#' @param rel_x column name with X coordinate of relative corner position in `x`
#' @param rel_y column name with Y coordinate of relative corner position in `x`
#' @param loc_x optional column name with X coordinate of corner position in
#'     `x`, only required if `x` is not an SF object 
#  @param loc_y optional column name with Y coordinate of corner position in
#      `x`, only required if `x` is not an SF object
#' @param dim dimensions of subplots, either a single value for square
#'     subplots, a vector of length two for rectangular subplots with given 
#'     width and length, or a list of vectors, one for each plot in the same
#'     row order as `x`.
#' @param align alignment of subplots relative to `x`. "north" = true north
#'     grid overlay, "centre" = centre of plot in orientation of plot edge, "SW", 
#'     "NW", "NE", or "SE" = to corner of plot in orientation of plot edge.
#'     Either a single value, a vector of values one for each plot in the same
#'     row order as `x`, or an sf object containing points defining the
#'     alignment in the same row order as `x` 
#' @param warp logical, if true, the subplots can be warped to fit a
#'     non-perfect polygon, relying instead on the stated plot size in `rel_x`
#'     and `rel_y`
#' @param angle optional vector of angle value describing plot orientation
#'     along bearing edge, in radians. Only needed if align != "north". Either
#'     a single value or a vector of values, one for each plot in the same row
#'     order as `x`. 
#' @param name optional column names in `x` to include in output 
#'
#' @return 
#' 
#' @importFrom sf st_make_grid st_sf st_centroid st_cast st_combine st_union st_geometry st_crs st_intersection st_area st_drop_geometry 
#' @importFrom dplyr bind_rows left_join
#' @importFrom units drop_units
#' 
#' @export
#' 
subplotSplit <- function(x, rel_x, rel_y, loc_x = NULL, loc_y = NULL, 
  dim, align = "north", warp = FALSE, angle = NULL, name = NULL) { 

  # Check arguments
  if (!is.null(name) && any(!name %in% colnames(x))) {
    stop("All values in 'name' must be columns in 'x'")
  }

  if (any(!c(rel_x, rel_y) %in% colnames(x))) {
    stop("'rel_x' and 'rel_y' must be columns in 'x'")
  }

  if (!isSFType(x) & (is.null(loc_x) | is.null(loc_y))) {
    stop("If 'x' is not an sf object, 'loc_x' and 'loc_y' must be provided")
  }

  if (!length(dim) %in% c(1, 2, nrow(x))) {
    stop("length of 'dim' must be 1, 2, or the number of rows in 'x'")
  }

  if (!length(align) %in% c(1, nrow(x))) {
    stop("length of 'align' must be 1, or the number of rows in 'x'")
  }

  if (!is.null(angle) && !length(angle) %in% c(1, nrow(x))) {
    stop("length of 'angle' must be 1, or the number of rows in 'x'")
  }

  if (!all(is.numeric(unlist(dim)))) {
    stop("all values in 'dim' must be numeric")
  }

  if (!isSFType(x, c("POINT"))) { 
    stop("'x' must be an sf object containing only POINT")
  }

  if (!all(align %in% c("north", "centre", "SW", "NW", "NE", "SE"))) {
    stop("all values in 'align' must be 'north', 'centre', 'SW', 'NW', 'NE', or 'SE'")
  }

  # Repeat dimensions if necessary
  if (length(dim) == 1) { 
    dim <- rep(dim, 2)
  }

  if (!is.list(dim)) {
    dim <- rep(list(dim), nrow(x))
  }

  # Repeat align if necessary
  if (length(align) == 1) {
    align <- rep(align, nrow(x))
  }

  # Repeat angles if necessary
  if (length(angle) == 1) {
    angle <- rep(angle, nrow(x))
  }

  # For each polygon
  out <- dplyr::bind_rows(lapply(seq_len(nrow(x)), function(i) {

    if (align[i] == "north") { 

      xg <- sf::st_make_grid(x[i,], cellsize = dim[[i]])

      g <- sf::st_sf(st_drop_geometry(x[rep(i, times = length(xg)),]), geometry = xg)

    } else if (align[i] %in% c("centre", "SW", "NW", "NE", "SE")) {

      # Rotate polygon so bearing edge is E-W
      xr <- rotation(x[i,], sf::st_centroid(x[i,]), angle[i])

      # Make grid
      xg <- sf::st_make_grid(xr, cellsize = dim[[i]])

      # Rotate grid back
      xgc <- sf::st_sf(geometry = sf::st_cast(sf::st_combine(xg), "MULTIPOLYGON"))
      xgr <- sf::st_cast(rotation(xgc, sf::st_centroid(xgc), -angle[i]), "POLYGON")
    }

    if (align[i] == "centre") { 

      # Find centroid of grid
      xgu <- sf::st_centroid(sf::st_union(xgr))

      # Find centroid of plot
      xc <- sf::st_geometry(sf::st_centroid(x[i,]))
      
      # Find offset
      cdiff <- xc - xgu

      # Apply offset to centre grid
      sf::st_geometry(xgr) <- sf::st_geometry(xgr) + cdiff 

      # Add CRS
      g <- xgr
      sf::st_crs(g) <- sf::st_crs(x)

    } else if (align[i] %in% c("SW", "NW", "NE", "SE")) {

      # Find selected corner of grid
      xgs <- polyCornerExtract(sf::st_sf(sf::st_union(xgr)), align[i])

      # Find selected corner of plot
      xs <- polyCornerExtract(x[i,], align[i])

      # Find offset
      cdiff <- sf::st_geometry(xs) - sf::st_geometry(xgs)

      # Apply offset to centre grid
      sf::st_geometry(xgr) <- sf::st_geometry(xgr) + cdiff 

      # Add CRS
      g <- xgr
      sf::st_crs(g) <- sf::st_crs(x)

    }

    gid <- cbind(row_id = seq_len(nrow(g)), g)

    # Calculate intersecting area of subplot polygons
    int <- sf::st_intersection(gid, x)
    int$int_area <- units::drop_units(sf::st_area(int)) * 0.0001

    # Calculate area of subplot polygons
    gid$area <- units::drop_units(sf::st_area(gid)) * 0.0001

    # Join intersecting and total area
    gint <- dplyr::left_join(
      gid[,c(names(gid)[1], "area")], 
      sf::st_drop_geometry(int[,c(names(int)[1], "int_area")]),
      by = names(gid)[1])
    gint$int_area[is.na(gint$int_area)] <- 0

    # Calculate proportional coverage to four decimal places (1 m^2)
    gint$area_prop <- round(gint$int_area, 4) / round(gint$area, 4)

    # Exclude subplots with less than 1 m^2 overlap
    gfil <- gint[gint$area_prop > 0,!names(gint) == "row_id"]

    # Add ID values
    if (!is.null(name)) {
      gfil <- st_sf(data.frame(sf::st_drop_geometry(x[i,name]), gfil))
    }

    # Add subplot IDs
    gout <- cbind(subplot_id = seq_len(nrow(gfil)), gfil)

    gout
  }))

  # Return
  return(out)
}

#' Convert grid coordinates to global coordinate system within a plot polygon
#'
#' @param x sf object containing stem locations in relative XY grid
#'     coordinates, either a dataframe for a single plot, or a list of
#'     dataframes
#' @param p sf object containing plot polygons, in the same row order as
#'     dataframes in `x`
#' @param origin sf object origins of rotation, probably plot corners, in the
#'     same row order as dataframes in `x`
#' @param angle angle values to rotate with, in radians, probably angles of
#'     rotation for a plot, in the same row order as dataframes in `x`
#' @param name optional column names in `x` to include in output
#'
#' @importFrom sf st_centroid st_as_sf st_coordinates st_crs st_sf st_as_sfc
#' @importFrom stringi stri_trans_general stri_replace_all_regex stri_count_words
#'
#' @export
#' 
globalCoord <- function(x, p, origin, angle, name = NULL) {

  # If not a list, make a list
  if (!inherits(x, "list")) {
    x <- list(x)
  }

  # Check arguments
  if (!all(unlist(lapply(x, isSFType, "POINT")))) { 
    stop("'x' must be an sf object containing only POINT")
  }

  if (nrow(p) != length(x)) {
    stop("Number of geometries in `p` must match number of stem dataframes in `x`")
  }

  if (nrow(origin) != length(x)) {
    stop("Number of geometries in `origin` must match number of stem dataframes in `x`")
  }

  if (length(angle) != length(x)) {
    stop("Number of values in `angle` must match number of stem dataframes in `x`")
  }

  if (!isSFType(p, c("POLYGON", "MULTIPOLYGON"))) { 
    stop("'p' must be an sf object containing only POLYGON or MULTIPOLYGON")
  }

  # For each stem dataframe
  out <- lapply(seq_along(x), function(i) {
    # Get centre of polygon
    cent <- sf::st_geometry(sf::st_centroid(p[i,]))

    # Get origin corner from rotated polygon
    or <- rotation(origin[i,], cent, angle[i])

    # Add origin global rotated coordinate to stem grid coordinates
    xg <- sf::st_as_sf(
      as.data.frame(sweep(sf::st_coordinates(x[[i]]), 2, sf::st_coordinates(or), `+`)), 
      coords = c(1,2), crs = sf::st_crs(p[i,]))

    # Rotate global coordinates back
    xgr <- rotation(xg, cent, -angle[i])

    # Construct output sf object
    sf::st_sf(x[[i]][,name], geometry = sf::st_as_sfc(xgr, crs = sf::st_crs(x[[i]])))
  })

  # Return
  return(out)
}

#' Assign stem measurements to subplots given location
#'
#' @param x sf object containing stem locations in global coordinates, as
#'     produced by `globalCoord()` for a single plot, or a list containing data 
#'     from multiple plots
#' @param p sf object containing the subplot polygons of a single plot, or
#'     multiple plots as a list in the same order as `x`
#' @param name names of columns in `x` and `p` to return in output. 
#'
#' @return 
#' 
#' @examples
#' 
#' @export
#' 
subplotAssign <- function(x, p, name) { 

  # If not a list, make a list
  if (!inherits(x, "list")) {
    x <- list(x)
  }

  if (!inherits(p, "list")) {
    p <- list(p)
  }

  # Check arguments
  if (length(x) != length(p)) {
    stop("Length of `x` must equal length of `p`")
  }

  if (!all(unlist(lapply(x, isSFType, "POINT")))) { 
    stop("'x' must be an sf object containing only POINT")
  }

  if (!all(unlist(lapply(p, isSFType, "POLYGON")))) { 
    stop("'p' must be an sf object containing only POLYGON")
  }

  # For each plot
  out <- lapply(seq_along(x), function(i) {

    # Check arguments
    if (any(!name %in% c(colnames(x[[i]]), colnames(p[[i]])))) {
      stop("All values in 'name' must be columns in 'p' or 'x'")
    }

    # Assign stem measurements to subplots
    sf::st_join(x[[i]], p[[i]], sf::st_within,
      suffix = c("", ".y")) %>% 
      dplyr::select(tidyselect::all_of(name))
  })

  # Return
  return(out)
}

#' Summarise biomass to spatial subsets
#'
#' @param x stem data
#' @param group name of columns containing spatial groups over which to
#'     summarise, e.g. plot ID and subplot ID 
#' @param area column name with area of spatial unit (ha)
#' @param agb column name with above-ground woody biomass of stems (Mg)
#' @param ba column name with stem basal area (m^2)
#' @param wd column name with stem wood density (g cm^-3)
#'
#' @return 
#' 
#' @examples
#' 
#' @export
#' 
subplotSumm <- function(x, group, area, agb, ba, wd) {
  x %>% 
    group_by(across(all_of(group))) %>% 
    summarise(
      area = first(.data[[area]]),
      agb_ha = sum(.data[[agb]], na.rm = TRUE) / area,
      wd_ba_wm = weighted.mean(.data[[wd]], .data[[ba]]),
      ba_ha = sum(.data[[ba]], na.rm = TRUE) / area)
}

#' Measure polygon edge lengths
#'
#' @param x sf object containing polygons
#'
#' @return list of unit vectors with lengths of all edges in each polygon
#' 
#' @export
#' 
polyEdgeLength <- function(x) { 
  if (!isSFType(x, "POLYGON")) {
    stop("'x' must be an sf object containing only POLYGONS")
  }

  # For each polygon
  out <- lapply(seq_len(nrow(x)), function(i) {
    # Extract coordinates matrix
    cds <- st_coordinates(x[i,])[,1:2]

    # Create edge combinations
    n <- nrow(cds)
    edges <- cbind(cds[1:(n-1), ], cds[2:n, ])

    # Convert to LINESTRING
    lines_sf <- st_sfc(lapply(seq_len(nrow(edges)), function(y) { 
      st_linestring(matrix(c(edges[y,1], edges[y,2], edges[y,3], edges[y,4]), 
         ncol = 2, byrow = TRUE))
    }), crs = st_crs(x))

    # Calculate length
    st_length(lines_sf)
  })

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

# Generate wood density estimates 
#
# @param x dataframe of stem data 
# @param wd_data dataframe of wood density data
# @param regional either logical, should wood density estimates be calculated
#     using wood density data from the same region only? OR a character vector
#     containing `wd_region` values to use from `wd_data`
# @param measurement_id column name of measurement IDs in \code{x}
# @param site_id column name of dataset IDs in \code{x}
# @param plot_id column name of plot IDs in \code{x}
# @param family column name of family names in \code{x} and \code{wd_data}
# @param genus column name of genera in \code{x} and \code{wd_data}
# @param species column name of species epithets in \code{x} 
#     and \code{wd_data}
# @param wd column name of wood density estimates in \code{wd_data}
# @param wd_region column name of wood density regions in \code{x} and
#     \code{wd_data}
#
# @return dataframe of wood density estimates for each row in \code{x}
# 
wdGen <- function(x, wd_data, regional = FALSE,
  measurement_id, site_id, plot_id, family, genus, species, wd, wd_region) {
  
  # If regional is a character vector of regions, ensure regions in wd_data
  if (is.character(regional) && !all(regional %in% wd_data[[wd_region]])) {
    warning("Regions missing from 'wd_data': ", 
      paste(regional[!regional %in% wd_data[[wd_region]]], collapse = ", "))
  }

  # Filter wood density data to chosen regions
  if (is.character(regional)) {
    wd_data <- wd_data %>% 
      filter(wd_region %in% regional)
    wd_data[[wd_region]] <- paste(regional, collapse = ";")
    x[[wd_region]] <- paste(regional, collapse = ";")
  }

  # If not regional then use all data
  if (!regional) {
    wd_data[[wd_region]] <- "world"
    x[[wd_region]] <- "world"
  }

  # Calculate reference uncertainty values for levels in wood density data
  species_sd <- wd_data %>% 
    group_by(.data[[wd_region]], .data[[family]], .data[[genus]], .data[[species]]) %>% 
    filter(n() > 10) %>% 
    summarise(sdWD = sd(wd), .groups = "drop") %>%
    group_by(.data[[wd_region]]) %>% 
    summarise(sdWD_ref = mean(sdWD), .groups = "drop") %>% 
    filter(!is.na(wd_region)) %>% 
    mutate(levelWD = "species")

  genus_sd <- wd_data %>% 
    group_by(.data[[wd_region]], .data[[family]], .data[[genus]]) %>% 
    filter(n() > 10) %>% 
    summarise(sdWD = sd(wd), .groups = "drop") %>%
    group_by(.data[[wd_region]]) %>% 
    summarise(sdWD_ref = mean(sdWD), .groups = "drop") %>% 
    filter(!is.na(wd_region)) %>% 
    mutate(levelWD = "genus")
      
  family_sd <- wd_data %>% 
    group_by(.data[[wd_region]], .data[[family]]) %>% 
    filter(n() > 10) %>% 
    summarise(sdWD = sd(wd), .groups = "drop") %>%
    group_by(.data[[wd_region]]) %>% 
    summarise(sdWD_ref = mean(sdWD), .groups = "drop") %>% 
    filter(!is.na(wd_region)) %>% 
    mutate(levelWD = "family")

  uncert <- do.call(bind_rows, list(species_sd, genus_sd, family_sd))

  # Get unique family, genus, species from stem data
  x_un <- unique(x[,c(family, genus, species, wd_region)])

  # Calculate all family, genus, species level means
  wd_species <- wd_data %>%
    group_by(.data[[wd_region]], .data[[family]], .data[[genus]], .data[[species]]) %>% 
    summarise(
      nInd = n(),
      meanWD = mean(.data[[wd]]),
      sdWD = sd(.data[[wd]]), .groups = "drop") %>%
    left_join(., uncert[uncert$levelWD == "species",], by = wd_region)

  wd_genus <- wd_data %>%
    group_by(.data[[family]], .data[[genus]], .data[[wd_region]]) %>% 
    summarise(
      nInd = n(),
      meanWD = mean(.data[[wd]]),
      sdWD = sd(.data[[wd]]), .groups = "drop") %>%
    left_join(., uncert[uncert$levelWD == "genus",], by = wd_region)

  wd_family <- wd_data %>%
    group_by(.data[[family]], .data[[wd_region]]) %>% 
    summarise(
      nInd = n(),
      meanWD = mean(.data[[wd]]),
      sdWD = sd(.data[[wd]]), .groups = "drop") %>%
    left_join(., uncert[uncert$levelWD == "family",], by = wd_region)

  # Sequentially fill in gaps
  x_wd_taxa <- x_un %>%
    left_join(., wd_species, by = c(family, genus, species, wd_region)) %>%
    rows_patch(., wd_genus, by = c(family, genus, wd_region), 
      unmatched = "ignore") %>%
    rows_patch(., wd_family, by = c(family, wd_region), 
      unmatched = "ignore")

  # Add wood density data to stem data
  x_wd <- x %>% 
    left_join(., x_wd_taxa, by = c(wd_region, family, genus, species))

  # Calculate plot mean wood density
  wd_plot <- x_wd %>% 
    group_by(.data[[plot_id]]) %>% 
    summarise(
      nInd = sum(!is.na(meanWD)),
      sdWD = sd(meanWD, na.rm = TRUE),
      meanWD = mean(meanWD, na.rm = TRUE)) %>% 
    mutate(
      sdWD_ref = sdWD,
      levelWD = "plot")

  # Calculate dataset mean wood density
  wd_dataset <- x_wd %>% 
    group_by(.data[[site_id]]) %>% 
    summarise(
      nInd = sum(!is.na(meanWD)),
      sdWD = sd(meanWD, na.rm = TRUE),
      meanWD = mean(meanWD, na.rm = TRUE)) %>% 
    mutate(
      sdWD_ref = sdWD,
      levelWD = "dataset")

  # Sequentially fill in gaps
  x_wd_all <- x_wd %>%
    rows_patch(wd_plot, by = plot_id) %>%
    rows_patch(wd_dataset, by = site_id)

  # Where SD is zero or unknown, use SD of entire wood density dataset
  x_wd_all$sdWD_ref <- ifelse(
    is.na(x_wd_all$sdWD_ref) | x_wd_all$sdWD_ref == 0, 
    sd(wd_data$wd), x_wd_all$sdWD_ref)

  # Reorder by measurement ID
  out <- x_wd_all %>% 
    arrange(measurement_id) %>% 
    dplyr::select(all_of(measurement_id), nInd, meanWD, sdWD, sdWD_ref, levelWD)

  # Check, no rows added or lost
  stopifnot(nrow(out) == nrow(x))

  # Return dataframe
  return(out)
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
