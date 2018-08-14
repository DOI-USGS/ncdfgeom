
.onAttach <- function(libname, pkgname) {
	packageStartupMessage("This information is preliminary or provisional 
and is subject to revision. It is being provided 
to meet the need for timely best science. The 
information has not received final approval by the 
U.S. Geological Survey (USGS) and is provided on the 
condition that neither the USGS nor the U.S. Government 
shall be held liable for any damages resulting from the 
authorized or unauthorized use of the information.

****Support Package****
This package is a USGS-R Support package. 
see: https://owi.usgs.gov/R/packages.html#support")
}

pkg.env <- new.env()
pkg.env$multi_val <- 0
pkg.env$hole_val <- 1

pkg.env$instance_dim_name <- "instance"

# Arbitrary dim and variable names assumed in code.
pkg.env$node_dim_name <- "node"
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

check_geomData <- function(geomData) {
	if (any(c("sf", "sfc") %in% class(geomData))) {
		geomData <- sf::as_Spatial(geomData)
	}
	return(geomData)
}
