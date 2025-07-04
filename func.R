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

  if (!is.null(corner) && !all(corner %in% c("SW", "NW", "NE", "SE"))) {
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

    if (!is.null(corner) && !is.na(corner[[i]])) { 
      xc$sum <- xc$X + xc$Y
      xc$diff <- xc$X - xc$Y
      xc$label <- NA_character_
      xc$label[which.min(xc$sum)] <- "SW"
      xc$label[which.max(xc$diff)] <- "SE"
      xc$label[which.max(xc$sum)] <- "NE"
      xc$label[which.min(xc$diff)] <- "NW"

      # Select chosen corner coordinate(s)
      xs <- xc[xc$label %in% sort(corner[[i]]) & !is.na(xc$label), 1:2]
      xs$corner_id <- sort(corner[[i]])
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
#' @param loc_x optional column name with X coordinate of corner position in `x`, only required if `x` is not an SF object
#' @param loc_y optional column name with Y coordinate of corner position in `x`, only required if `x` is not an SF object
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





