
.onAttach <- function(libname, pkgname) {
	packageStartupMessage("****Support Package****
This package is a USGS-R Support package. 
see: https://owi.usgs.gov/R/packages.html#support")
}

pkg.env <- new.env()
pkg.env$multi_val <- 0
pkg.env$hole_val <- 1

# Arbitrary dim and variable names assumed in code.
pkg.env$instance_dim_name <- "instance"
pkg.env$time_dim_name <- "time"
pkg.env$time_var_name <- "time"
pkg.env$lat_coord_var_name <- "lat"
pkg.env$lon_coord_var_name <- "lon"
pkg.env$alt_coord_var_name <- "alt"
pkg.env$str_len_dim <- "name_strlen"
pkg.env$dsg_timeseries_id <- "instance_name"
pkg.env$node_dim_name <- "node"
pkg.env$x_nodes <- "x_nodes"
pkg.env$y_nodes <- "y_nodes"
pkg.env$part_dim_name <- "part"
pkg.env$part_node_count_var_name <- "part_node_count"
pkg.env$part_type_var_name <- "interior_ring"
pkg.env$node_count_var_name <- "node_count"
pkg.env$geom_container_var_name <- "geometry_container"
pkg.env$crs_var_name <- "grid_mapping"

# Variables prescribed in the specification.
pkg.env$cf_version <- "CF-1.8"
pkg.env$x_axis <- "X"
pkg.env$y_axis <- "Y"
pkg.env$node_coordinates <- "node_coordinates"
pkg.env$geom_type_attr_name <- "geometry_type"
pkg.env$node_count_attr_name <- "node_count"
pkg.env$part_node_count_attr_name <- "part_node_count"
pkg.env$part_type_attr_name <- "interior_ring"
pkg.env$geometry_container_att_name <- "geometry"
pkg.env$crs <- "grid_mapping"
pkg.env$time_var_standard_name <- "time"
pkg.env$lat_coord_var_standard_name <- "latitude"
pkg.env$lon_coord_var_standard_name <- "longitude"
pkg.env$alt_coord_var_standard_name <- "height"
pkg.env$timeseries_id_cf_role <- "timeseries_id"

pkg.env$nc_types <- list(double = "NC_DOUBLE", float = "NC_FLOAT", numeric="NC_DOUBLE", short = "NC_SHORT", integer = "NC_INT", char="NC_CHAR", character="NC_CHAR")

#' @importFrom sf st_as_sf
check_geom_data <- function(geom_data) {
	if (!any(c("sf", "sfc") %in% class(geom_data))) {
		geom_data <- sf::st_as_sf(geom_data)
	}
	return(geom_data)
}

add_var <- function(nc, name, dim, type, units = NA, missing = NA, long_name = NA, char_dim_len = NULL, data = NULL) {
  
  if(type == "NC_CHAR") {
    suppressWarnings(if(is.null(char_dim_len) & is.null(data)) stop("can't determine character dim length"))
    if(is.null(char_dim_len)) suppressWarnings(char_dim_len <- max(sapply(data, function(x) max(nchar(x), 
                                                                             na.rm = TRUE)), 
                                                  na.rm = TRUE))
    char_dim <- paste0(name,"_char")
    dim.def.nc(nc, char_dim, char_dim_len, unlim = FALSE)
    dim <- c(char_dim, dim)
  }
    var.def.nc(nc, name, type, dim)
    if(!any(is.na(units)))
      att.put.nc(nc, name, "units", "NC_CHAR", units)
    if(!is.na(missing))
      att.put.nc(nc, name, "missing_value", type, missing)
    if(!is.na(long_name))
      att.put.nc(nc, name, "long_name", "NC_CHAR", long_name)
}
