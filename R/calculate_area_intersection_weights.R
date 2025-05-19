#' Area Weighted Intersection
#' @description Returns the fractional percent of each
#' feature in x that is covered by each intersecting feature
#' in y. These can be used as the weights in an area-weighted
#' mean overlay analysis where x is the data **source** and area-
#' weighted means are being generated for the **target**, y.
#'
#' This function is a light wieght wrapper around the functions
#' \link[areal]{aw_intersect} \link[areal]{aw_total} and \link[areal]{aw_weight}
#' from the \href{https://chris-prener.github.io/areal/}{areal package}.
#'
#' @param x sf data.frame source features including one geometry column and one identifier column
#' @param y sf data.frame target features including one geometry column and one identifier column
#' @param normalize logical return normalized weights or not. 
#' 
#' Normalized weights express the fraction of **target** polygons covered by 
#' a portion of each **source** polygon. They are normalized in that the area
#' of each **source** polygon has already been factored into the weight.
#' Normalized weights are intended to be used with _intensive_ variables.
#' 
#' Un-normalized weights express the fraction of **source** polygons covered by
#' a portion of each **target** polygon. This is a more general form that requires
#' knowledge of the area of each **source** polygon to derive area-weighted
#' statistics from **source** to **target. Un-normalized weights are intended
#' to be used with either _intensive_ or _extensive_ variables.
#' 
#' See details and examples for more regarding this distinction.
#' 
#' @param allow_lonlat boolean If FALSE (the default) lon/lat target features are not allowed.
#' Intersections in lon/lat are generally not valid and problematic at the international date line.
#' 
#' @return data.frame containing fraction of each feature in x that is
#' covered by each feature in y. 
#' 
#' @details
#' 
#' Two versions of weights are available: 
#' 
#' `normalize = FALSE`, if a polygon from x (source) is entirely within a polygon in y
#' (target), w will be 1. If a polygon from x (source) is 50% in one polygon from y (target) 
#' and 50% in another, there will be two rows, one for each x/y pair of features with w = 0.5 
#' in each. Weights will sum to 1 per **SOURCE** polygon if the target polygons fully cover that 
#' feature.
#' 
#' For `normalize = FALSE` the area weighted mean calculation must include the area of each 
#' x (source) polygon as in:
#' 
#' > *in this case, `area` is the area of source polygons and you would do this operation grouped 
#' by target polygon id.*
#'
#' > `sum( (val * w * area), na.rm = TRUE ) / sum(w * area)`
#' 
#' If `normalize = TRUE`, weights are divided by the target polygon area such that weights
#' sum to 1 per TARGET polygon if the target polygon is fully covered by source polygons.
#' 
#' For `normalize = FALSE` the area weighted mean calculation no area is required
#' as in:
#' 
#' > `sum( (val * w), na.rm = TRUE ) / sum(w)`
#'
#' See examples for illustration of these two modes.
#'
#' @examples
#'
#' library(sf)
#' 
#' source <- st_sf(source_id = c(1, 2), 
#'                 val = c(10, 20), 
#'                 geom = st_as_sfc(c(
#'   "POLYGON ((0.2 1.2, 1.8 1.2, 1.8 2.8, 0.2 2.8, 0.2 1.2))", 
#'   "POLYGON ((-1.96 1.04, -0.04 1.04, -0.04 2.96, -1.96 2.96, -1.96 1.04))")))
#' 
#' source$area <- as.numeric(st_area(source))
#' 
#' target <- st_sf(target_id = "a", 
#'                 geom = st_as_sfc("POLYGON ((-1.2 1, 0.8 1, 0.8 3, -1.2 3, -1.2 1))"))
#' 
#' plot(source['val'], reset = FALSE)
#' plot(st_geometry(target), add = TRUE)
#' 
#' (w <- 
#' calculate_area_intersection_weights(source[c("source_id", "geom")], 
#'                                     target[c("target_id", "geom")], 
#'                                     normalize = FALSE, allow_lonlat = TRUE))
#' 
#' (res <-
#' merge(st_drop_geometry(source), w, by = "source_id"))
#' 
#' (intesive <- sum(res$val * res$w * res$area) / sum(res$w * res$area))
#' (extensive <- sum(res$val * res$w))
#' 
#' (w <-
#' calculate_area_intersection_weights(source[c("source_id", "geom")], 
#'                                     target[c("target_id", "geom")], 
#'                                     normalize = TRUE, allow_lonlat = TRUE))
#' (res <-
#' merge(st_drop_geometry(source), w, by = "source_id"))
#' 
#' (intensive <- sum(res$val * res$w) / sum(res$w))
#'
#' @export
#' @importFrom sf st_intersection st_set_geometry st_area st_crs st_drop_geometry
#' @importFrom dplyr mutate group_by right_join select ungroup left_join mutate

calculate_area_intersection_weights <- function(x, y, normalize, allow_lonlat = FALSE) {
 
  if(missing(normalize)) {
    warning("Required input normalize is missing, defaulting to FALSE.")
    normalize <- FALSE
  }

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
                             areaVar = "area_intersection")
  
  if(!normalize) {

    int <- areal::aw_total(int, 
                           source = x,
                           id = "varx", # the unique id in the "source" x
                           areaVar = "area_intersection",
                           totalVar = "totalArea_x",
                           type = "extensive",
                           weight = "total")
    
    int <- areal::aw_weight(int, areaVar = "area_intersection",
                            totalVar = "totalArea_x",
                            areaWeight = "areaWeight_x_y")
    
  } else {

    # for normalized, we sum the intersection area by the total target area
    int <- left_join(int, data.frame(vary = y$vary, 
                                     totalArea_y = as.numeric(sf::st_area(y))), by = "vary")
    
    int <- areal::aw_weight(int, 
                            areaVar = "area_intersection",
                            totalVar = "totalArea_y",
                            areaWeight = "areaWeight_x_y")
    
  }
  
  int <- right_join(st_drop_geometry(int), st_drop_geometry(x), by = "varx")
  
  int <- select(int, varx, vary, w = "areaWeight_x_y")
  
  names(int) <- c(id_x, id_y, "w")

  return(dplyr::as_tibble(int))
}

varx <- vary <- w <- d <- poly_id <- areaWeight <- NULL
