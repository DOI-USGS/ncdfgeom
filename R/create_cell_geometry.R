#' Create Cell Geometry
#'
#' @description Creates cell geometry from vectors of X and Y positions.
#'
#' @param X_coords numeric center positions of X axis indices
#' @param Y_coords numeric center positions of Y axis indices
#' @param prj character proj4 string for x and y
#' @param geom sf data.frame with geometry that cell geometry should cover
#' @param buffer_dist numeric a distance to buffer the cell geometry in units of geom projection
#' @param regularize boolean if TRUE, grid spacing will be adjusted to be exactly
#' equal. Only applies to 1-d coordinates.
#' @param eps numeric sets tolerance for grid regularity.
#'
#' @details Intersection is performed with cell centers then geometry is constructed.
#' A buffer may be required to fully cover geometry with cells.
#'
#' @importFrom sf st_sf st_as_sf st_transform st_buffer st_join st_contains
#' "st_crs<-" st_union st_bbox
#' @importFrom dplyr filter
#' @importFrom stars st_as_stars st_dimensions
#' @export
#' @examples
#' nc <- RNetCDF::open.nc(system.file("extdata/metdata.nc", package = "intersectr"))
#' ncmeta::nc_vars(nc)
#' variable_name <- "precipitation_amount"
#' cv <- ncmeta::nc_coord_var(nc, variable_name)
#'
#' x <- RNetCDF::var.get.nc(nc, cv$X, unpack = TRUE)
#' y <- RNetCDF::var.get.nc(nc, cv$Y, unpack = TRUE)
#'
#' prj <- ncmeta::nc_gm_to_prj(ncmeta::nc_grid_mapping_atts(nc))
#'
#' geom <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
#' geom <- sf::st_transform(geom, 5070)
#'
#' cell_geometry <- create_cell_geometry(x, y, prj, geom, 0)
#'
#' plot(sf::st_geometry(cell_geometry), lwd = 0.25)
#' plot(sf::st_transform(sf::st_geometry(geom), prj), add = TRUE)
#'
create_cell_geometry <- function(X_coords, Y_coords, prj,
                                 geom = NULL, buffer_dist = 0,
                                 regularize = FALSE, eps = 1e-10) {

  i_ind <- j_ind <- NULL
  dateline <- FALSE
  lonlat <- st_crs(prj)$proj == "longlat"

  if (!is.array(X_coords) || length(dim(X_coords)) == 1) {
    #### Vector Coordinate Variables
    if(lonlat & any(X_coords > 180)) {
      warning("Found longitude greater than 180. Converting to -180, 180")
      X_coords[X_coords > 180] <- X_coords[X_coords > 180] - 360
    }

    if(lonlat & min(X_coords) < -170) {
      warning("Found longidude near international date line. Using 0-360 longitude.")
      X_coords[X_coords < 0] <- X_coords[X_coords < 0] + 360
      if(!is.null(geom)) {
        geom <- st_transform(geom, prj)
        geom <- make_zero_360(geom)
      }
      dateline <- TRUE
    }

    if (!check_regular(X_coords, eps) | !check_regular(Y_coords, eps)) {
      if(regularize) {
        X_coords <- make_regular(X_coords, lonlat)
        Y_coords <- make_regular(Y_coords, lonlat)
      } else {
        stop("Found irregular grid. Only regular rasters or curvilinear grids supported")
      }
    }

    if(!is.null(geom)) {
      geom <- st_buffer(st_union(geom), buffer_dist)
      if(!dateline) geom <- st_transform(geom, prj)
      req_bbox <- st_bbox(geom)

      # Grab stuff in bounding box.
      i_ind <- which(X_coords > req_bbox$xmin & X_coords < req_bbox$xmax)
      j_ind <- which(Y_coords > req_bbox$ymin & Y_coords < req_bbox$ymax)

      if(length(i_ind) == 0 | length(j_ind) == 0)
        stop("Data and geometry not found to intersect. Check projection?")

      X_coords <- X_coords[i_ind]
      Y_coords <- Y_coords[j_ind]
    }

    sf_points <- construct_points(X_coords, Y_coords, i_ind, j_ind, prj)

    sf_polygons <- get_ids(length(X_coords), length(Y_coords))
    dim(sf_polygons) <- c(x = length(X_coords), y = length(Y_coords))

    sf_polygons <- st_as_stars(list(sf_polygons = sf_polygons),
                               dimensions = st_dimensions(x = as.numeric(X_coords),
                                                          y = as.numeric(Y_coords),
                                                          .raster = c("x", "y"),
                                                          cell_midpoints = TRUE))

  } else {
  #### Array Coordinate Variables
    if(!is.null(geom)) {
      geom <- st_buffer(st_union(geom), buffer_dist)
      if(!dateline) geom <- st_transform(geom, prj)
      req_bbox <- st_bbox(geom)

      warning("Found curvilinear grid, caution, limited testing.")
      X_inds <- which(X_coords > req_bbox$xmin & X_coords < req_bbox$xmax, arr.ind = TRUE)
      Y_inds <- which(Y_coords > req_bbox$ymin & Y_coords < req_bbox$ymax, arr.ind = TRUE)

      matches <- dplyr::intersect(as.data.frame(X_inds), as.data.frame(Y_inds))

      i_ind <- seq(min(matches[, 1]), max(matches[, 1]), 1)
      j_ind <- seq(min(matches[, 2]), max(matches[, 2]), 1)

      X_coords <- X_coords[i_ind, j_ind]
      Y_coords <- Y_coords[i_ind, j_ind]
    }
    sf_points <-construct_points_array(X_coords, Y_coords, i_ind, j_ind, prj)

    i_dim_len <- nrow(X_coords)
    j_dim_len <- ncol(X_coords)

    sf_polygons <- get_ids(i_dim_len, j_dim_len)

    dim(sf_polygons) <- c(x = i_dim_len, y = j_dim_len)

    sf_polygons <- st_as_stars(list(sf_polygons = sf_polygons),
                               dimensions = st_dimensions(x = seq(1:i_dim_len),
                                                          y = seq(1:j_dim_len),
                                                          .raster = c("x", "y"),
                                                          cell_midpoints = TRUE))

    sf_polygons <- st_as_stars(sf_polygons, curvilinear = list(x = X_coords,
                                                               y = Y_coords))
  }

  st_crs(sf_polygons) <- st_crs(prj)

  sf_polygons <- st_as_sf(sf_polygons, as_points = FALSE)

  names(sf_polygons)[1] <- "grid_ids"
  sf_polygons$grid_ids <- as.integer(sf_polygons$grid_ids)

  sf_polygons <- st_join(sf_polygons, sf_points, join = st_contains)

  sf::st_agr(sf_polygons) <- "constant"

  return(sf_polygons)
}

construct_points <- function(x, y, i_ind = NULL, j_ind = NULL, prj) {

  x_vals <- matrix(x, nrow = length(y), ncol = length(x),
                   byrow = T)
  y_vals <- matrix(y, nrow = length(y), ncol = length(x),
                   byrow = F)

  if(is.null(i_ind)) {
    i_ind <- c(1:ncol(x_vals))
    j_ind <- c(1:nrow(x_vals))
  }

  get_points(x_vals, y_vals, i_ind, j_ind, prj)
}

construct_points_array <- function(x, y, i_ind = NULL, j_ind = NULL, prj) {

  if(is.null(i_ind)) {
    i_ind <- matrix(rep(c(1:ncol(x)), nrow(x)),
                    nrow = nrow(x), ncol = ncol(x),
                    byrow = TRUE)

    j_ind <- matrix(rep(c(1:nrow(x)), ncol(x)),
                    nrow = nrow(x), ncol = ncol(x),
                    byrow = FALSE)
  }
  x_vals <- x
  y_vals <- y

  get_points(x_vals, y_vals, i_ind, j_ind, prj)

}

get_points <- function(x_vals, y_vals, i_ind, j_ind, prj) {
  if(length(i_ind) == nrow(x_vals)) {
    j_ind_m <- matrix(rep(i_ind, ncol(x_vals)),
                    nrow = nrow(x_vals), ncol = ncol(x_vals),
                    byrow = FALSE)
    i_ind_m <- matrix(rep(j_ind, nrow(x_vals)),
                    nrow = nrow(x_vals), ncol = ncol(x_vals),
                    byrow = TRUE)
  } else {
    if(!is.matrix(i_ind)) {
      j_ind_m <- matrix(rep(i_ind, nrow(x_vals)),
                             nrow = nrow(x_vals), ncol = ncol(x_vals),
                             byrow = TRUE)
      i_ind_m <- matrix(rep(j_ind, ncol(x_vals)),
                             nrow = nrow(x_vals), ncol = ncol(x_vals),
                             byrow = FALSE)
    } else {
      i_ind_m <- i_ind
      j_ind_m <- j_ind
    }
  }

  sf_points <- st_as_sf(data.frame(x = matrix(x_vals, ncol = 1),
                                   y = matrix(y_vals, ncol = 1),
                                   X_ind = matrix(j_ind_m, ncol = 1),
                                   Y_ind = matrix(i_ind_m, ncol = 1)),
                        coords = c("x", "y"),
                        crs = prj,
                        agr = "constant")
}

#' get data source array IDs
#'
#' Use IDs to map a NetCDF data source array onto cell geometry IDs
#'
#' @param X_dim_size size of dimension containing X coordinates (typically columns)
#' @param Y_dim_size size of dimension containing Y coordinates (typically rows)
#' @noRd
#' @examples
#' get_ids(5, 7)
#' matrix(get_ids(5,7), ncol = 1, byrow = FALSE)

get_ids <- function(X_dim_size, Y_dim_size) {
  matrix(as.numeric(seq(1, X_dim_size * Y_dim_size)),
         nrow = X_dim_size, ncol = Y_dim_size,
         byrow = FALSE)
}

which_within <- function(d, digits = 4) {
  abs(round(d - mean(d[1], d[length(d)]), digits))
}

check_regular <- function(x, eps = 1e-4) {
  d <- diff(x)
  # Deal with date line.
  cd <- which_within(d, round(-(log(eps, base = 10))))
  if(sum(cd > 0) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

make_regular <- function(x, lonlat) {
  return(seq(from = x[1],
             to = x[length(x)],
             along.with = x))
}

#' @importFrom sf st_polygon st_sfc st_union st_convex_hull st_coordinates
make_zero_360 <- function(geom) {
  prj <- st_crs(geom)
  geom <- st_convex_hull(st_union(st_transform(geom, 3832)))
  coords <- st_coordinates(st_transform(geom, prj))

  x <- coords[, 1]
  x[x < 0] <- x[x < 0] + 360

  coords[, 1] <- x

  st_sfc(st_polygon(list(coords[, c(1:2)])), crs = st_crs(geom))
}
