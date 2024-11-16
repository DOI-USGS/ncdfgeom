#' Area Weighted Intersection (areal implementation)
#' @description Returns the fractional percent of each
#' feature in x that is covered by each intersecting feature
#' in y. These can be used as the weights in an area-weighted
#' mean overlay analysis where x is the data **source** and area-
#' weighted means are being generated for the **target**, y.
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
#' If `normalize = FALSE`, if a polygon from x (source) is entirely within a polygon in y
#'  (target), w will be 1. If a polygon from x (source) is 50% in one polygon from y (target) 
#'  and 50% in another, there will be two rows, one for each x/y pair of features with w = 0.5 
#'  in each. Weights will sum to 1 per **SOURCE** polygon if the target polygons fully cover that 
#'  feature.
#'  
#'  For `normalize = FALSE` the area weighted mean calculation must include the area of each 
#'  x (source) polygon as in:
#'  
#'  > *in this case, `area` is the area of source polygons and you would do this operation grouped 
#'  by target polygon id.*
#'
#'  > `sum( (val * w * area), na.rm = TRUE ) / sum(w * area)`
#'  
#' If `normalize = TRUE`, weights are divided by the target polygon area such that weights
#' sum to 1 per TARGET polygon if the target polygon is fully covered by source polygons.
#' 
#'  For `normalize = FALSE` the area weighted mean calculation no area is required
#'  as in:
#' 
#'  > `sum( (val * w), na.rm = TRUE ) / sum(w)`
#'
#'  See examples for illustration of these two modes.
#'
#' @examples
#' 
#' library(dplyr)
#' library(sf)
#' library(ncdfgeom)
#' 
#' g <- list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1)))
#' 
#' a1 = sf::st_polygon(g) * 0.8
#' a2 = a1 + c(1, 2)
#' a3 = a1 + c(-1, 2)
#' 
#' b1 = sf::st_polygon(g)
#' b2 = b1 + 2
#' b3 = b1 + c(-0.2, 2)
#' b4 = b1 + c(2.2, 0)
#' 
#' a = sf::st_sfc(a1,a2,a3)
#' 
#' b = sf::st_sfc(b1, b2, b3, b4)
#' 
#' plot(c(a,b), border = NA)
#' plot(a, border = 'darkgreen', add = TRUE)
#' plot(b, border = 'red', add = TRUE)
#' 
#' a <- sf::st_sf(a, data.frame(ida = c(1, 2, 3)))
#' b <- sf::st_sf(b, data.frame(idb = c(7, 8, 9, 10)))
#' 
#' text(sapply(sf::st_geometry(a), \(x) mean(x[[1]][,1]) + 0.4),
#'      sapply(sf::st_geometry(a), \(x) mean(x[[1]][,2]) + 0.3),
#'      a$ida, col = "darkgreen")
#'      
#' text(sapply(sf::st_geometry(b), \(x) mean(x[[1]][,1]) + 0.4),
#'      sapply(sf::st_geometry(b), \(x) mean(x[[1]][,2])),
#'      b$idb, col = "red")
#'
#' sf::st_agr(a) <- sf::st_agr(b) <- "constant"
#' sf::st_crs(b) <- sf::st_crs(a) <- sf::st_crs(5070)
#'
#' calculate_area_intersection_weights(a, b, normalize = FALSE)
#' 
#' # NOTE: normalize = FALSE so weights sum to 1 per source polygon 
#' # when source is fully within target.
#' 
#' calculate_area_intersection_weights(a, b, normalize = TRUE)
#' 
#' # NOTE: normalize = TRUE so weights sum to 1 per target polygon. Non-overlap
#' # is ignored as if it does not exist.
#' 
#' calculate_area_intersection_weights(b, a, normalize = FALSE)
#' 
#' # NOTE: normalize = FALSE so weights never sum to 1 since no source is fully
#' # within target.
#' 
#' calculate_area_intersection_weights(b, a, normalize = TRUE)
#' 
#' # NOTE: normalize = TRUE so weights sum to 1 per target polygon. Non-overlap 
#' # is ignored as if it does not exist.
#' 
#' # a more typical arrangement of polygons
#' 
#' g <- list(rbind(c(-1,-1), c(1,-1), c(1,1), 
#'                 c(-1,1), c(-1,-1)))
#' 
#' a1 = st_polygon(g) * 0.75 + c(-.25, -.25)
#' a2 = a1 + 1.5
#' a3 = a1 + c(0, 1.5)
#' a4 = a1 + c(1.5, 0)
#' 
#' b1 = st_polygon(g)
#' b2 = b1 + 2
#' b3 = b1 + c(0, 2)
#' b4 = b1 + c(2, 0)
#' 
#' a = st_sfc(a1,a2, a3, a4)
#' b = st_sfc(b1, b2, b3, b4)
#' 
#' b <- st_sf(b, data.frame(idb = c(1, 2, 3, 4)))
#' a <- st_sf(a, data.frame(ida = c(6, 7, 8, 9)))
#' 
#' plot(st_geometry(b), border = 'red', lwd = 3)
#' plot(st_geometry(a), border = 'darkgreen', lwd = 3, add = TRUE)
#' 
#' text(sapply(st_geometry(a), \(x) mean(x[[1]][,1]) + 0.4),
#'      sapply(st_geometry(a), \(x) mean(x[[1]][,2]) + 0.3),
#'      a$ida, col = "darkgreen")
#' 
#' text(sapply(st_geometry(b), \(x) mean(x[[1]][,1]) - 0.4),
#'      sapply(st_geometry(b), \(x) mean(x[[1]][,2]) - 0.5),
#'      b$idb, col = "red")
#' 
#' st_agr(a) <- st_agr(b) <- "constant"
#' st_crs(b) <- st_crs(a) <- st_crs(5070)
#' 
#' a$val <- c(1, 2, 3, 4)
#' a$a_areasqkm <- 1.5 ^ 2
#' 
#' plot(a["val"], reset = FALSE)
#' plot(st_geometry(b), border = 'red', lwd = 3, add = TRUE, reset = FALSE)
#' plot(st_geometry(a), border = 'darkgreen', lwd = 3, add = TRUE)
#' 
#' text(sapply(st_geometry(a), \(x) mean(x[[1]][,1]) + 0.4),
#'      sapply(st_geometry(a), \(x) mean(x[[1]][,2]) + 0.3),
#'      a$ida, col = "darkgreen")
#' 
#' text(sapply(st_geometry(b), \(x) mean(x[[1]][,1]) - 0.4),
#'      sapply(st_geometry(b), \(x) mean(x[[1]][,2]) - 0.5),
#'      b$idb, col = "red")
#' 
#' # say we have data from `a` that we want sampled to `b`.
#' # this gives the percent of each `a` that intersects each `b`
#' 
#' (a_b <- calculate_area_intersection_weights(
#'   select(a, ida), select(b, idb), normalize = FALSE))
#' 
#' # NOTE: `w` sums to 1 per `a` in all cases
#' 
#' summarize(group_by(a_b, ida), w = sum(w))
#' 
#' # Since normalize is false, we apply weights like:
#' st_drop_geometry(a) |>
#'   left_join(a_b, by = "ida") |>
#'   mutate(a_areasqkm = 1.5 ^ 2) |> # add area of each polygon in `a`
#'   group_by(idb) |> # group so we get one row per `b`
#'   # now we calculate the value for each b with fraction of the area of each 
#'   # polygon in `a` per polygon in `b` with an equation like this:
#'   summarize(
#'     new_val = sum( (val * w * a_areasqkm), na.rm = TRUE ) / sum(w * a_areasqkm))
#' 
#' # NOTE: `w` is the fraction of the polygon in a. We need to multiply w by the 
#' # unique area of the polygon it is associated with to get the weighted mean weight.
#' 
#' # we can go in reverse if we had data from b that we want sampled to a
#' 
#' (b_a <- calculate_area_intersection_weights(
#'   select(b, idb), select(a, ida), normalize = FALSE))
#' 
#' # NOTE: `w` sums to 1 per `b` (source) only where `b` is fully covered by `a` (target).
#' 
#' summarize(group_by(b_a, idb), w = sum(w))
#' 
#' # Now let's look at what happens if we set normalize = TRUE. Here we 
#' # get `a` as source and `b` as target but normalize the weights so
#' # the area of a is built into `w`.
#' 
#' (a_b <- calculate_area_intersection_weights(
#'   select(a, ida), select(b, idb), normalize = TRUE))
#' 
#' # NOTE: if we summarize by `b` (target) `w` sums to 1 where above, with 
#' #       normalize = FALSE, `w` summed to one per `a` (source).
#' 
#' summarize(group_by(a_b, idb), w = sum(w))
#' 
#' # Since normalize is false, we apply weights like:
#' st_drop_geometry(a) |>
#'   left_join(a_b, by = "ida") |>
#'   group_by(idb) |> # group so we get one row per `b`
#'   # now we weight by the percent of each polygon in `b` per polygon in `a`
#'   summarize(new_val = sum( (val * w), na.rm = TRUE ))
#' 
#' # NOTE: `w` is the fraction of the polygon from `a` overlapping the polygon from `b`. 
#' # The area of `a` is built into the weight so we just sum the weith times value oer polygon.
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
