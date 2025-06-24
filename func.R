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
  inherits(x, c("sf", "sfc")) && 
    all(sf::st_geometry_type(x, by_geometry = FALSE) %in% type) | is.null(type)
}

#' Extract specific corner coordinates from sf polygons
#'
#' @param x sf object containing plot polygons, assumes all are rectangular
#' @param origin vector of corner direction values either "SW", "NW", "NE", "SE"
#' @param name optional column names in `x` to include in output
#'
#' @return sf object containing corner points 
#' 
#' @importFrom dplyr bind_rows
#' @importFrom sf st_coordinates st_union st_sf st_sfc st_point st_crs
#' 
#' @export
#' 
polyCornerExtract <- function(x, corner = "SW", name = NULL) { 

  # Check arguments
  if (!is.null(name) && any(!name %in% colnames(x))) {
    stop("All values in 'name' must be columns in 'x'")
  }

  if (!length(corner) %in% c(1, nrow(x))) {
    stop("length of 'corner' must be 1 or the number of rows in 'x'")
  }

  if (!all(corner %in% c("SW", "NW", "NE", "SE"))) {
    stop("all values in 'corner' must be 'SW', 'NW', 'NE', or 'SE'")
  }

  if (!isSFType(x, c("POLYGON", "MULTIPOLYGON"))) { 
    stop("'x' must be an sf object containing only POLYGON or MULTIPOLYGON")
  }

  # Repeat corner direction if necessary
  if (length(corner) == 1) { 
    corner <- rep(corner, nrow(x))
   }

  # For each polygon
  out <- dplyr::bind_rows(lapply(seq_len(nrow(x)), function(i) {
    # Isolate polygon
    xsel <- x[i,]

    # Extract corner coordinates
    xc <- as.data.frame(sf::st_coordinates(sf::st_union(xsel)))
    xc$sum <- xc$X + xc$Y
    xc$diff <- xc$X - xc$Y
    xc$label <- NA_character_
    xc$label[which.min(xc$sum)] <- "SW"
    xc$label[which.max(xc$diff)] <- "SE"
    xc$label[which.max(xc$sum)] <- "NE"
    xc$label[which.min(xc$diff)] <- "NW"

    # Select chosen corner coordinate
    xs <- unlist(xc[xc$label == corner[i] & !is.na(xc$label), 1:2])

    # Return selected corner coordinate(s)
    sf::st_sf(x[i,name], 
      geometry = sf::st_sfc(sf::st_point(xs), 
      crs = sf::st_crs(x)))
  }))

  # Return
  return(out)
}

#' Perform a rotation on an sf object
#'
#' @param x sf object
#' @param origin sf object with origin of rotation 
#' @param angle vector of angle values, in radians
#' 
#' @return
#'
#' @importFrom sf st_geometry st_coordinates st_geometry_type st_polygon st_multipolygon st_linestring st_multilinestring st_point st_multipoint st_sf st_sfc st_crs
#'
#' @export
#' 
rotation <- function(x, origin, angle) { 

  # Check arguments
  if (!length(angle) %in% c(1, nrow(x))) {
    stop("length of 'angle' must be 1 or the number of rows in 'x'")
  }

  if (!all(is.numeric(angle))) {
    stop("all values in 'angle' must be numeric")
  }

  if (!isSFType(x)) { 
    stop("'x' must be an sf object")
  }

  if (!isSFType(origin, "POINT")) {
    stop("'origin' must be an sf object containing only POINT")
  }

  if (!length(sf::st_geometry(origin)) %in% c(1, nrow(x))) {
    stop("number of geometries in 'origin' must be 1 or the number of rows in 'x'")
  }

  # Repeat corner direction if necessary
  if (length(angle) == 1) { 
    angle <- rep(angle, nrow(x))
   }

  # Repeat origins of rotation if necessary
  if (length(sf::st_geometry(origin)) == 1) { 
    origin <- rep(sf::st_geometry(origin), nrow(x))
   }

  # Extract geometries
  xg <- sf::st_geometry(x)

  # Create empty list to fill with rotated geometries
  rg <- vector("list", length = length(xg))

  # Define helper functions to do rotation
  rot <- function(a) { 
    matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2) 
  }

  rot_fn <- function(g, o, a) {
    om <- matrix(o, nrow(g), 2, byrow = TRUE)
    ((g - om) %*% rot(a)) + om
  }

  # For each geometry
  for (i in seq_along(xg)) {

    # Extract geometries
    g <- xg[[i]]
    o <- sf::st_coordinates(origin[i, ])
    a <- angle[i]

    # Handle geometry types: POLYGON and MULTIPOLYGON
    rg[[i]] <- switch(as.character(sf::st_geometry_type(g)),
      POLYGON = sf::st_polygon(lapply(g, rot_fn, o, a)),
      MULTIPOLYGON = sf::st_multipolygon(lapply(g, function(p) lapply(p, rot_fn, o, a))),
      LINESTRING = sf::st_linestring(rot_fn(g, o, a)),
      MULTILINESTRING = sf::st_multilinestring(lapply(g, rot_fn, o, a)),
      POINT = sf::st_point(rot_fn(matrix(g, nrow = 1), o, a)),
      MULTIPOINT = sf::st_multipoint(rot_fn(g, o, a)),
      stop("Unsupported geometry type")
    )
  }

  # Construct output sf object
  out <- sf::st_sf(x, geometry = sf::st_sfc(rg, crs = sf::st_crs(x)))

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

#' Split a plot into regular subplots
#'
#' @param x sf object containing plot polygons 
#' @param dim dimensions of subplots, either a single value for square
#'     subplots, a vector of length two for rectangular subplots with given 
#'     width and length, or a list of vectors, one for each plot in the same
#'     row order as `x`.
#' @param TODO: align alignment of subplots relative to `x`. "north" = true north
#'     grid overlay, "centre" = centre of plot in orientation of plot edge, "SW", 
#'     "NW", "NE", or "SE" = to corner of plot in orientation of plot edge.
#'     Either a single value, a vector of values one for each plot in the same
#'     row order as `x`, or an sf object containing points defining the
#'     alignment in the same row order as `x` 
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
subplotSplit <- function(x, dim, align = "north", angle = NULL, name = NULL) { 

  # Check arguments
  if (!is.null(name) && any(!name %in% colnames(x))) {
    stop("All values in 'name' must be columns in 'x'")
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

  if (!isSFType(x, c("POLYGON"))) { 
    stop("'x' must be an sf object containing only POLYGON")
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
#' @param x sf object containing stem locations in relative XY grid coordinates, either a dataframe for a single plot, or a list of dataframes
#' @param p sf object containing plot polygons, in the same row order as dataframes in `x`
#' @param origin sf object origins of rotation, probably plot corners, in the same row order as dataframes in `x`
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
#' @param area area of spatial unit (ha)
#' @param agb above-ground woody biomass of stems (Mg)
#' @param ba stem basal area (m^2)
#' @param wd stem wood density (g cm^-3)
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

