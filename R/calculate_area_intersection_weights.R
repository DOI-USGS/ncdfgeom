#' Area Weighted Intersection (areal implementation)
#' @description Returns the fractional percent of each
#' feature in x that is covered by each intersecting feature
#' in y. These can be used as the weights in an area-weighted
#' mean overlay analysis where x is the data source and area-
#' weighted means are being generated for the target, y.
#'
#' This function is a lightwieght wrapper around the functions
#' \link[areal]{aw_intersect} \link[areal]{aw_total} and \link[areal]{aw_weight}
#' from the \href{https://chris-prener.github.io/areal/}{areal package}.
#'
#' @param x sf data.frame source features including one geometry column and one identifier column
#' @param y sf data.frame target features including one geometry column and one identifier column
#' @param normalize logical return normalized weights or not. See details and examples.
#' @param allow_lonlat boolean If FALSE (the default) lon/lat target features are not allowed.
#' Intersections in lon/lat are generally not valid and problematic at the international date line.
#' @return data.frame containing fraction of each feature in x that is
#' covered by each feature in y. 
#' 
#' @details
#' 
#' Two versions of weights are available: 
#' 
#' `normalize = FALSE`, if a polygon from x is entirely within a polygon in y,
#' w will be 1. If a polygon from x is 50% in one polygon from y and 50% in another, there
#' will be two rows, one for each x/y pair of features with w = 0.5 in each. Weights
#' will sum to 1 per SOURCE polygon if the target polygons fully cover that feature.
#' `normalize = TRUE`, weights are divided by the target polygon area such that weights
#' sum to 1 per TARGET polygon if the target polygon is fully covered by source polygons.
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
#' calculate_area_intersection_weights(a, b, normalize = FALSE)
#' calculate_area_intersection_weights(a, b, normalize = TRUE)
#' calculate_area_intersection_weights(b, a, normalize = FALSE)
#' calculate_area_intersection_weights(b, a, normalize = TRUE)
#' 
#' #a more typical arrangement of polygons
#' 
#' b1 = sf::st_polygon(list(rbind(c(-1,-1), c(1,-1),
#'                            c(1,1), c(-1,1),
#'                            c(-1,-1))))
#' b2 = b1 + 2
#' b3 = b1 + c(0, 2)
#' b4 = b1 + c(2, 0)
#' b = sf::st_sfc(b1, b2, b3, b4)
#' a1 = b1 * 0.75 + c(-.25, -.25)
#' a2 = a1 + 1.5
#' a3 = a1 + c(0, 1.5)
#' a4 = a1 + c(1.5, 0)
#' a = sf::st_sfc(a1,a2,a3, a4)
#' plot(b, border = 'red', lwd = 3)
#' plot(a, border = 'green', add = TRUE)
#' 
#' sf::st_crs(b) <- sf::st_crs(a) <- sf::st_crs(5070)
#'
#' b <- sf::st_sf(b, data.frame(idb = c(1, 2, 3, 4)))
#' a <- sf::st_sf(a, data.frame(ida = c("a", "b", "c", "d")))
#'
#' sf::st_agr(a) <- sf::st_agr(b) <- "constant"
#'
#' # say we have data from `a` that we want sampled to `b`.
#' # this gives the percent of each `a` that intersects each `b`
#' 
#' (a_b <- calculate_area_intersection_weights(a, b, normalize = FALSE))
#' 
#' # note that `w` sums to 1 where `b` completely covers `a`.
#' 
#' dplyr::summarize(dplyr::group_by(a_b, ida), w = sum(w))
#' 
#' # We can apply these weights like...
#' dplyr::tibble(ida = unique(a_b$ida), 
#'                    val = c(1, 2, 3, 4)) |>
#'   dplyr::left_join(a_b, by = "ida") |>
#'   dplyr::mutate(val = ifelse(is.na(w), NA, val),
#'                 areasqkm = 1.5 ^ 2) |> # area of each polygon in `a`
#'   dplyr::group_by(idb) |> # group so we get one row per `b`
#'   # now we weight by the percent of the area from each polygon in `b` per polygon in `a`
#'   dplyr::summarize(new_val = sum( (val * w * areasqkm), na.rm = TRUE ) / sum(w * areasqkm))
#'   
#' # we can go in reverse if we had data from b that we want sampled to a
#' 
#' (b_a <- calculate_area_intersection_weights(b, a, normalize = FALSE))
#' 
#' # note that `w` sums to 1 only where `a` complete covers `b`
#' 
#' dplyr::summarize(dplyr::group_by(b_a, idb), w = sum(w))
#' 
#' # with `normalize = TRUE`, `w` will sum to 1 when the target 
#' # completely covers the source rather than when the source completely
#' # covers the target. 
#' 
#' (a_b <- calculate_area_intersection_weights(a, b, normalize = TRUE))
#' 
#' dplyr::summarize(dplyr::group_by(a_b, idb), w = sum(w))
#' 
#' (b_a <- calculate_area_intersection_weights(b, a, normalize = TRUE))
#' 
#' dplyr::summarize(dplyr::group_by(b_a, ida), w = sum(w))
#' 
#' # We can apply these weights like...
#' # Note that we don't need area in the normalized case
#' dplyr::tibble(ida = unique(a_b$ida), 
#'                    val = c(1, 2, 3, 4)) |>
#'   dplyr::left_join(a_b, by = "ida") |>
#'   dplyr::mutate(val = ifelse(is.na(w), NA, val)) |>
#'   dplyr::group_by(idb) |> # group so we get one row per `b`
#'   # now we weight by the percent of the area from each polygon in `b` per polygon in `a`
#'   dplyr::summarize(new_val = sum( (val * w), na.rm = TRUE ))

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
  int <- areal::aw_total(int, source = x,
                         id = "varx", # the unique id in the "source" x
                         areaVar = "area_intersection",
                         totalVar = "totalArea_x",
                         type = "extensive",
                         weight = "total")
  int <- areal::aw_weight(int, areaVar = "area_intersection",
                          totalVar = "totalArea_x",
                          areaWeight = "areaWeight_x_y")

  int <- right_join(st_drop_geometry(int), st_drop_geometry(x), by = "varx")

  if(normalize) {
    
    # for normalized, we return the percent of each target covered by each source
    int <- areal::aw_intersect(y,
                               source = x,
                               areaVar = "area_intersection")
    
    # for normalized, we sum the intersection area by the total target intersection area
    int <- ungroup(mutate(group_by(int, vary), totalArea_y = sum(area_intersection)))
    
    int <- areal::aw_weight(int, 
                            areaVar = "area_intersection",
                            totalVar = "totalArea_y",
                            areaWeight = "areaWeight_x_y")
    
  }
  
  int <- right_join(st_drop_geometry(int), st_drop_geometry(x), by = "varx")
  
  int <- select(int, varx, vary, w = areaWeight_x_y)

  names(int) <- c(id_x, id_y, "w")

  return(dplyr::as_tibble(int))
}

varx <- vary <- w <- d <- poly_id <- areaWeight <- NULL
