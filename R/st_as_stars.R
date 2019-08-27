#' Convert ncdfgeom object into stars object.
#' @importFrom stars st_as_stars
#' @param .x Object of class ncdfgeom as returned by read_timeseries_dsg.
#' @param ... not used.
#' @param sf_geometry sf data.frame with geometry and attributes to be added to stars object. 
#' Must have same number of rows as timeseries instances.
#' @name st_as_stars
#' @export
#' 
st_as_stars.ncdfgeom <- function(.x, ..., sf_geometry = NA) {
  crs <- st_crs(4326)$proj4string
  ts_points <- data.frame(X = .x$lons, Y = .x$lats, Z = .x$alts)
  ts_points <- sf::st_as_sf(ts_points, coords = c("X", "Y", "Z"), crs = crs)
  
  data <- .x$data_frames[[1]]
  # data[["T"]] <- .x$time
  
  gdim <- stars:::create_dimension(from = 1, to = length(.x$lats), 
                                   refsys = crs, point = TRUE, 
                                   values = ts_points$geometry)
  tdim <- stars:::create_dimension(from = 1, to = length(.x$time), 
                                   refsys = "POSIXct", point = FALSE, 
                                   values = as.POSIXct(.x$time))
  dim <- list(time = tdim, points = gdim)
  
  if("sf" %in% class(sf_geometry)) {
    if(length(gdim$values) != length(st_geometry(sf_geometry))) 
      stop("geometry must be same length as instance dimension of timeseries")
    
    is_point <- any(grepl("point", class(st_geometry(sf_geometry)), ignore.case = TRUE))
    
    sf_dim <- stars:::create_dimension(from = 1, to = length(gdim$values),
                                       refsys = st_crs(sf_geometry)$proj4string, 
                                       point = is_point, is_raster = FALSE,
                                       values = st_geometry(sf_geometry))
    
    dim <- c(dim, list(geometry = sf_dim))
  }
  
  stars:::st_stars(x = setNames(list(as.matrix(.x$data_frames[[1]])), 
                                .x$varmeta[[1]]$name), 
                   dimensions =  stars:::create_dimensions(dim))
}
