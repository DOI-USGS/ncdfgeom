#' Area Weighted Intersection (areal implementation)
#' @description Returns the fractional percent of each
#' feature in x that is covered by each intersecting feature
#' in y. These can be used as the weights in an area-weighted
#' mean overlay analysis where x is the data source and area-
#' weighted means are being generated for the target, y.
#'
#' This function is a lightwieght wrapper around the functions
#' \link[areal]{aw_intersect} \link[areal]{aw_total} and \link[areal]{aw_weight}
#' from the \href{https://github.com/slu-openGIS/areal}{areal package}.
#'
#' @param x sf data.frame source features including one geometry column and one identifier column
#' @param y sf data.frame target features including one geometry column and one identifier column
#' @param allow_lonlat boolean If FALSE (the default) lon/lat target features are not allowed.
#' Intersections in lon/lat are generally not valid and problematic at the international date line.
#' @return data.frame containing fraction of each feature in x that is
#' covered by each feature in y. e.g. If a feature from x is entirely within a feature in y,
#' w will be 1. If a feature from x is 50% in one feature for y and 50% in another, there
#' will be two rows, one for each x/y pair of features with w = 0.5 in each.
#'
#' @examples
#' b1 = sf::st_polygon(list(rbind(c(-1,-1), c(1,-1),
#'                            c(1,1), c(-1,1),
#'                            c(-1,-1))))
#' b2 = b1 + 2
#' b3 = b1 + c(-0.2, 2)
#' b4 = b1 + c(2.2, 0)
#' b = sf::st_sfc(b1, b2, b3, b4)
#' a1 = b1 * 0.8
#' a2 = a1 + c(1, 2)
#' a3 = a1 + c(-1, 2)
#' a = sf::st_sfc(a1,a2,a3)
#' plot(b, border = 'red')
#' plot(a, border = 'green', add = TRUE)
#'
#' sf::st_crs(b) <- sf::st_crs(a) <- sf::st_crs(5070)
#'
#' b <- sf::st_sf(b, data.frame(idb = c(1, 2, 3, 4)))
#' a <- sf::st_sf(a, data.frame(ida = c(1, 2, 3)))
#'
#' sf::st_agr(a) <- sf::st_agr(b) <- "constant"
#'
#' a_b <- calculate_area_intersection_weights(a, b)
#' b_a <- calculate_area_intersection_weights(b, a)
#'
#' @export
#' @importFrom sf st_intersection st_set_geometry st_area st_crs
#' @importFrom dplyr mutate group_by right_join select ungroup

calculate_area_intersection_weights <- function(x, y, allow_lonlat = FALSE) {

  if(!requireNamespace("areal")) stop("areal package required for intersection weights")
  
  if (st_crs(x) != st_crs(y)) {
    x <- st_transform(x, st_crs(y))
  }

  if(st_crs(y)$proj == "longlat" & !allow_lonlat) {
    stop("Found lon/lat coordinates and allow_lonlat is FALSE.")
  }

  # Standard evaluation is for chumps.
  id_x <- names(x)[names(x) != attr(x, "sf_column")]
  id_y <- names(y)[names(y) != attr(y, "sf_column")]

  # There is a bug in areal and this works around it.
  geom_name_x <- attr(x, "sf_column")
  attr(x, "sf_column") <- "geometry"
  names(x)[which(names(x) == geom_name_x)] <- "geometry"

  geom_name_y <- attr(y, "sf_column")
  attr(y, "sf_column") <- "geometry"
  names(y)[which(names(y) == geom_name_y)] <- "geometry"

  if (length(id_x) != 1 | length(id_y) != 1)
    stop("x and y must have one and only one non-geometry column")

  names(x)[names(x) == id_x] <- "varx"
  names(y)[names(y) == id_y] <- "vary"

  int <- areal::aw_intersect(y,
                             source = x,
                             areaVar = "area")
  int <- areal::aw_total(int, source = x,
                         id = "varx",
                         areaVar = "area",
                         totalVar = "totalArea",
                         type = "extensive",
                         weight = "total")
  int <- areal::aw_weight(int, areaVar = "area",
                          totalVar = "totalArea",
                          areaWeight = "areaWeight")

  int <- right_join(st_set_geometry(int, NULL), st_set_geometry(x, NULL), by = "varx")

  int <- select(int, varx, vary, w = areaWeight)

  names(int) <- c(id_x, id_y, "w")

  return(dplyr::as_tibble(int))
}

varx <- vary <- w <- d <- poly_id <- areaWeight <- NULL
