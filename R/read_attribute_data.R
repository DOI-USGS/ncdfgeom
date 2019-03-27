#'@title Read attribute dataframe from NetCDF-DSG file
#'
#'@param nc A NetCDF path or urlto be opened.
#'@param instance_dim The NetCDF instance/station dimension.
#'
#'@description
#'Gets attribute data from a NetCDF-DSG file and returns it in a \code{data.frame}.
#'This function is intended as a convenience to be used within workflows where
#'the netCDF file is already open and well understood.
#'
#' @export
#' @importFrom ncmeta nc_meta
#' @importFrom RNetCDF open.nc close.nc var.get.nc
#' @importFrom dplyr filter
#' @examples 
#' hucPolygons <- sf::read_sf(system.file('extdata','example_huc_eta.json', package = 'ncdfgeom'))
#' hucPolygons_nc <- ncdfgeom::write_geometry(tempfile(), hucPolygons)
#' 
#' read_attribute_data(hucPolygons_nc, "instance")
#'
read_attribute_data <- function(nc, instance_dim) {
  
  name <- variable <- NULL
  
  nc_meta <- nc_meta(nc)
	
  nc <- open.nc(nc)
  on.exit(close.nc(nc), add  = TRUE)
  
  nc_dim <- filter(nc_meta$dimension, name == instance_dim)
  
	if(nrow(nc_dim) == 0) {
		stop("The instance dimension was not found in the provided NetCDF object.")
	}
	  
  dataFrame <- as.data.frame(list(id = 1:nc_dim$length))
  
  for(i in 1:nrow(nc_meta$variable)) {
    var <- nc_meta$variable[i, ]
    axis <- filter(nc_meta$axis, variable == var$name)
    if(var$ndims == 1 && axis$dimension == nc_dim$id) {
      if(var$type == "NC_INT") {
        dataFrame[var$name] <- as.integer(c(var.get.nc(nc, var$name)))
      } else {
        dataFrame[var$name] <- c(var.get.nc(nc, var$name))
      }
    } else if(var$type == "NC_CHAR" &&
              (nc_dim$id %in% axis$dimension))
      dataFrame[var$name] <- tryCatch({
        c(var.get.nc(nc, var$name))
      }, error = function(e) {
        t(var.get.nc(nc, var$name)) 
      })
  }
  
  dataFrame[] <- lapply(dataFrame, make.true.NA)
  
  return(dataFrame)
}

# found here: http://stackoverflow.com/questions/26220913/replace-na-with-na
make.true.NA <- function(x) if(is.character(x) || is.factor(x)) {
	is.na(x) <- x=="NA"; x } else {
		x }