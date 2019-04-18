#' @title Write geometries and attributes to NetCDF-CF
#'
#' @param nc_file \code{character} file path to the nc file to be created.
#' @param geom_data sf \code{data.frame} with POINT, LINESTRING, MULTILINESTRING, 
#' POLYGON, or MULTIPOLYGON geometries. Note that three dimensional geometries 
#' are not supported. sp geometries will be coerced to sf with sf::as_Spatial.
#' @param instance_dim_name \code{character} Not required if adding geometry to a 
#' NetCDF-CF Discrete Sampling Geometries timeSeries file. For a new file, will 
#' use package default -- "instance" -- if not supplied.
#' @param variables \code{character} If a an existing netCDF files is provided, this 
#' list of variables that should be related to the geometries.
#'
#' @description
#' Creates a file with point, line or polygon instance data ready for the 
#' extended NetCDF-CF timeSeries featuretype format.
#' 
#' Will also add attributes if provided data has them.
#'
#'@references
#' \enumerate{
#'   \item \url{http://cfconventions.org/cf-conventions/cf-conventions.html}
#'}
#'
#' @importFrom sf st_set_geometry st_geometry
#' @export
#' 
#' @examples 
#'
#' hucPolygons <- sf::read_sf(system.file('extdata','example_huc_eta.json', package = 'ncdfgeom'))
#'
#' hucPolygons_nc <- ncdfgeom::write_geometry(nc_file=tempfile(), 
#'                                            geom_data = hucPolygons)
#' ncdump <- system(paste("ncdump -h", hucPolygons_nc), intern = TRUE)
#' cat(ncdump ,sep = "\n")
#' 
write_geometry = function(nc_file, geom_data, instance_dim_name = NULL, variables = list()) {

	geom_data <- check_geom_data(geom_data)

  type <- get_type(geom_data)
    
  if(grepl("MULTIPOINT", type)) {
  	stop("Multi point not supported yet.")
  } else if(!grepl("POLYGON", type) & 
            !grepl("LINESTRING", type) &
            type != "POINT") {
    stop("Did not find supported spatial data.")
  }
  
  att_data <- st_set_geometry(geom_data, NULL)
  geom_data <- st_geometry(geom_data)

  if(is.null(instance_dim_name)) {
    instance_dim_name <- pkg.env$instance_dim_name
  }

	if(ncol(att_data) > 0) {
		itemp <- sapply(att_data, is.factor)
		att_data[itemp] <- lapply(att_data[itemp], as.character)
		
		nc_file <- write_attribute_data(nc_file, att_data, instance_dim_name)
		
		variables <- c(variables, names(att_data))
	}

  return(write_geom_data(geom_data, nc_file, instance_dim_name, variables = variables))
}

#'@title Put geometry data in a NetCDF-CF File
#'
#'
#'@param nc_file A string file path to the nc file to be created. It must already have
#'an instance dimension.
#'@param geom_data An object of class \code{SpatialLines}, \code{SpatialPolygons}
#'or their sf with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that three dimensional geometries are not supported.
#'@param instance_dim_name A string to name the instance dimension.  Defaults to "instance"
#'@param variables A character vector of variable names that the geometry data
#'container variable name will be added to.

#'@description
#'Creates a file with point, line or polygon instance data ready for the extended NetCDF-CF timeSeries featuretype format.
#'Will also add attributes if a sp dataframe object is passed in.
#'
#'@references
#'https://github.com/twhiteaker/netCDF-CF-simple-geometry
#'
#' @importFrom RNetCDF open.nc close.nc create.nc var.put.nc att.put.nc
#' @importFrom stats setNames
#' @importFrom sf st_geometry_type st_crs st_coordinates
#' @noRd
write_geom_data <- function(geom_data, ...) 
  UseMethod("write_geom_data")

#' @noRd
#' @name write_geom_data
write_geom_data.sfc_POINT <- function(geom_data, nc_file, instance_dim_name, variables = c()) {
  crs <- get_crs(geom_data)
  
  geom_data <- st_coordinates(geom_data)
  
  node_count <- c(1:nrow(geom_data))
  
  x_vals <- geom_data[,1]
  y_vals <- geom_data[,2]
  
  node_dim_name <- instance_dim_name
  
  nc <- init_ncdf(nc_file, x_vals, y_vals, instance_dim_name)
  
  write_geom_POINT(nc)
  
  put_meta_nc(nc, crs, variables)
  
  close.nc(nc)
  
  return(nc_file)
}

#' @noRd
#' @name write_geom_data
write_geom_data.sfc_LINESTRING <- function(geom_data, nc_file, instance_dim_name, variables = c()) {
  crs <- get_crs(geom_data)
  
  geom_data <- st_coordinates(geom_data)
  
  n_geoms <- length(unique(geom_data[, 3]))
  
  node_count <- rep(NA, n_geoms)
  x_vals <- rep(NA, nrow(geom_data))
  y_vals <- x_vals
  coord <- 1
  
  for(geom in 1:n_geoms) {
    coords <- geom_data[geom_data[, 3] == geom, ][, c(1,2)]
    nrc <- nrow(coords)

    x_vals[coord:(coord + nrc - 1)] <- coords[,1]
    y_vals[coord:(coord + nrc - 1)] <- coords[,2]
    
    node_count[geom] <- nrc
    coord <- coord + nrc
  }
  
  nc <- init_ncdf(nc_file, x_vals, y_vals, pkg.env$node_dim_name)
  
  write_geom_LINESTRING(nc, node_count, instance_dim_name, NULL)
  
  put_meta_nc(nc, crs, variables)
  
  close.nc(nc)
  
  return(nc_file)
}

#' @noRd
#' @name write_geom_data
write_geom_data.sfc_MULTILINESTRING <- function(geom_data, nc_file, 
                                                instance_dim_name, variables = c()) {
  crs <- get_crs(geom_data)
  
  geom_data <- st_coordinates(geom_data)
  
  part_dif <- diff(geom_data[, 3])
  part_dif[part_dif < 0] <- 0
  geom_dif <- diff(geom_data[, 4])
  geom_data[, 3] <- c(1, cumsum(part_dif + geom_dif) + 1)
  
  n_geoms <- length(unique(geom_data[, 4]))
  n_parts <- length(unique(geom_data[, 3]))
  
  nCoords <- nrow(geom_data)
  
  part_node_count <- rep(NA, n_parts)
  node_count <- rep(NA, n_geoms)
  
  x_vals <- rep(NA, nCoords)
  y_vals <- x_vals
  nc_part <- 0
  coord <- 1
  
  for(geom in 1:n_geoms) {
    nd_count <- 0
    
    g_data <- geom_data[geom_data[, 4] == geom, ]

    for(g_part in 1:length(unique(g_data[, 3]))) {
      nc_part <- nc_part + 1
      
      coords <- g_data[g_data[, 3] == g_part, c(1,2)]
      nrc <- nrow(coords)
      
      x_vals[coord:(coord+nrc-1)] <- coords[,1]
      y_vals[coord:(coord+nrc-1)] <- coords[,2]
      
      nd_count <- nd_count + nrc
      part_node_count[nc_part] <- nrc
      coord <- coord + nrc
    }
    node_count[geom] <- nd_count
  }
  
  nc <- init_ncdf(nc_file, x_vals, y_vals, pkg.env$node_dim_name)
  
  write_geom_LINESTRING(nc, node_count, instance_dim_name, part_node_count)
  
  put_meta_nc(nc, crs, variables)
  
  close.nc(nc)
  
  return(nc_file)
}

#' @noRd
#' @name write_geom_data
write_geom_data.sfc_POLYGON <- function(geom_data, nc_file, instance_dim_name, variables = c()) {
  crs <- get_crs(geom_data)
  
  geom_data <- st_coordinates(geom_data)
  
  hole_dif <- diff(geom_data[, 3])
  hole_dif[hole_dif < 0] <- 0
  
  n_geoms <- length(unique(geom_data[, 4]))
  n_parts <- n_geoms + sum(hole_dif)
  
  nCoords <- nrow(geom_data)
  
  part_node_count <- rep(NA, n_parts)  
  part_type <- rep(pkg.env$multi_val, n_parts)
  node_count <- rep(NA, n_geoms)
  
  x_vals <- rep(NA, nCoords)
  y_vals <- x_vals
  nc_part <- 0
  coord <- 1
  
  for(geom in 1:n_geoms) {
    nd_count <- 0
    g_data <- geom_data[geom_data[, 4] == geom, ]
    
    nc_parts <- length(unique(g_data[, 3]))
    
    for(g_part in 1:nc_parts) {
      nc_part <- nc_part + 1
      
      if(g_part > 1) {
        part_type[nc_part] <- pkg.env$hole_val
      }
      
      coords <- g_data[g_data[, 3] == g_part, c(1,2)]
      nrc <- nrow(coords)
      
      x_vals[coord:(coord+nrow(coords)-1)]<-coords[1:nrow(coords), 1]
      y_vals[coord:(coord+nrow(coords)-1)]<-coords[1:nrow(coords), 2]
        
      nd_count <- nd_count + nrc
      part_node_count[nc_part] <-  nrc
      coord <- coord + nrc
    }
    node_count[geom] <- nd_count
  }
  
  nc <- init_ncdf(nc_file, x_vals, y_vals, pkg.env$node_dim_name)
  
  if(all(part_type == pkg.env$multi_val)) part_node_count <- part_type <- NULL
  
  write_geom_POLYGON(nc, node_count, instance_dim_name, part_node_count, part_type)
  
  put_meta_nc(nc, crs, variables)
  
  close.nc(nc)
  
  return(nc_file)
}

#' @noRd
#' @name write_geom_data
write_geom_data.sfc_MULTIPOLYGON <- function(geom_data, nc_file, instance_dim_name, variables = c()) {
  
  crs <- get_crs(geom_data)
  
  geom_data <- st_coordinates(geom_data)
  
  hole_dif <- diff(geom_data[, 3])
  hole_dif[hole_dif < 0] <- 0
  part_dif <- diff(geom_data[, 4])
  part_dif[part_dif < 0] <- 0
  geom_dif <- diff(geom_data[, 5])
  
  geom_data[, 4] <- c(1, cumsum(hole_dif + part_dif + geom_dif) + 1)
  
  n_geoms <- length(unique(geom_data[, 5]))
  n_parts <- length(unique(geom_data[, 4]))
  
  nCoords <- nrow(geom_data)
  
  part_node_count <- rep(NA, n_parts)  
  part_type <- rep(pkg.env$multi_val, n_parts)
  node_count <- rep(NA, n_geoms)
  
  x_vals <- rep(NA, nCoords)
  y_vals <- x_vals
  nc_part <- 0
  coord <- 1
  
    for(geom in 1:n_geoms) {
      nd_count <- 0
      g_data <- geom_data[geom_data[, 5] == geom, ]
      
      nc_parts <- length(unique(g_data[, 4]))
      
      for(g_part in 1:nc_parts) {
        nc_part <- nc_part + 1
        
        if(g_part > 1 && g_data[g_data[, 4] == nc_part, 3][1] > 1) {
          part_type[nc_part] <- pkg.env$hole_val
        } else {
          part_type[nc_part] <-pkg.env$multi_val
        }
        
        coords <- g_data[g_data[, 4] == nc_part, c(1,2)]
        nrc <- nrow(coords)

        x_vals[coord:(coord + nrc - 1)] <- coords[1:nrc, 1]
        y_vals[coord:(coord + nrc - 1)] <- coords[1:nrc, 2]
        
        nd_count <- nd_count + nrc
        part_node_count[nc_part] <-  nrc
        coord <- coord + nrc
      }
      node_count[geom] <- nd_count
    }
  
  nc <- init_ncdf(nc_file, x_vals, y_vals, pkg.env$node_dim_name)
  
  write_geom_POLYGON(nc, node_count, instance_dim_name, part_node_count, part_type)
  
  put_meta_nc(nc, crs, variables)
  
  close.nc(nc)
  
  return(nc_file)
}

get_crs <- function(geom_data) {
  crs <- ncmeta::nc_prj_to_gridmapping(st_crs(geom_data)$proj4string)
  crs <- setNames(crs$value, crs$name)
  
  if(length(crs) == 0) {
    crs <- list(grid_mapping_name = "latitude_longitude",
                semi_major_axis = 6378137,
                inverse_flattening = 298.257223563,
                longitude_of_prime_meridian = 0)
    warning("No CRS was found. Assuming WGS84 Lat Lon.")
  }
  return(crs)
}

get_type <- function(geom_data) {
  type <- unique(st_geometry_type(geom_data))
  
  if(length(type) > 1) stop("Found multiple geometry types, only one is supported.")
  
  return(type)
}

init_ncdf <- function(nc_file, x_vals, y_vals, node_dim_name) {
  if(file.exists(nc_file)) {
    nc <- open.nc(nc_file, write = TRUE)
  } else {
    nc <- create.nc(nc_file, large = TRUE)
  }
  
  try(dim.def.nc(nc, node_dim_name, length(x_vals), unlim = FALSE), silent = TRUE)
  
  var.def.nc(nc, pkg.env$x_nodes, "NC_DOUBLE", node_dim_name)
  var.def.nc(nc, pkg.env$y_nodes, "NC_DOUBLE", node_dim_name)
  
  att.put.nc(nc, pkg.env$x_nodes, "units", "NC_CHAR", "degrees_east")
  att.put.nc(nc, pkg.env$y_nodes, "units", "NC_CHAR", "degrees_north")
  
  close.nc(nc)
  nc <- open.nc(nc_file, write = TRUE)
  
  var.put.nc(nc, pkg.env$x_nodes, x_vals)
  var.put.nc(nc, pkg.env$y_nodes, y_vals)
  # att.put.nc(nc, pkg.env$x_nodes, 'standard_name', "NC_CHAR", 'longitude')
  # att.put.nc(nc, pkg.env$y_nodes, 'standard_name', "NC_CHAR", 'latitude')
  att.put.nc(nc, pkg.env$x_nodes, "axis", "NC_CHAR", "X")
  att.put.nc(nc, pkg.env$y_nodes, "axis", "NC_CHAR", "Y")
  
  geom_container <- var.def.nc(nc, pkg.env$geom_container_var_name, "NC_INT", NA)
  att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$node_coordinates, "NC_CHAR", paste(pkg.env$x_nodes, pkg.env$y_nodes))
  
  return(nc)
}

put_meta_nc <- function(nc, crs, variables) {
  if(length(crs) > 0) {
    att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$crs, "NC_CHAR", pkg.env$crs_var_name)
    
    for(var in variables) {
      att.put.nc(nc, var, pkg.env$crs, "NC_CHAR", pkg.env$crs_var_name)
    }
    
    crs_var <- var.def.nc(nc, pkg.env$crs_var_name, vartype = "NC_INT", dimensions = NA)
    
    # close.nc(nc)
    # 
    # nc <- open.nc(nc_file, write = TRUE)
    
    for(crs_att in names(crs)) att.put.nc(nc, pkg.env$crs_var_name, crs_att, pkg.env$nc_types[class(crs[crs_att][[1]])][[1]], crs[crs_att][[1]])
  }
  
  for(var in variables) {
    att.put.nc(nc, var, pkg.env$geometry_container_att_name, "NC_CHAR", pkg.env$geom_container_var_name)
  }
  
  att.put.nc(nc, "NC_GLOBAL", "Conventions", "NC_CHAR", pkg.env$cf_version)
  
  return(invisible(nc))
}

write_geom_POINT <- function(nc) {
  att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name, "NC_CHAR", 'point')
  return(invisible(nc))
}

write_geom_LINESTRING <- function(nc, node_count, instance_dim_name, part_node_count) {
  put_line(nc, node_count, instance_dim_name, part_node_count)
}

put_line <- function(nc, node_count, instance_dim_name, part_node_count) {
  att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name, "NC_CHAR", 'line')
  
  put_node_count(nc, node_count, instance_dim_name)
  
  if(!is.null(part_node_count)) put_part_node_count(nc, part_node_count)
}

write_geom_POLYGON <- function(nc, node_count, instance_dim_name, part_node_count, part_type) {
  put_poly(nc, node_count, instance_dim_name, part_node_count, part_type)
}

put_poly <- function(nc, node_count, instance_dim_name, part_node_count, part_type) {
  att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name, "NC_CHAR", 'polygon')
  
  put_node_count(nc, node_count, instance_dim_name)
  
  if(!is.null(part_node_count)) put_part_node_count(nc, part_node_count)
  
  if(!is.null(part_type)) put_hole_node_count(nc, part_type)
}

put_node_count <- function(nc, node_count, instance_dim_name) {
  att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$node_count_attr_name, "NC_CHAR", pkg.env$node_count_var_name)
  
  instance_dim <- tryCatch({
    dim.inq.nc(nc, instance_dim_name)},
    error = function(e) {
      dim.def.nc(nc, instance_dim_name, length(node_count), unlim = FALSE)
      dim.inq.nc(nc, instance_dim_name)
    })
  
  var.def.nc(nc, pkg.env$node_count_var_name, "NC_INT", instance_dim$id)
  att.put.nc(nc, pkg.env$node_count_var_name, "long_name", "NC_CHAR", "count of coordinates in each instance geometry")
  var.put.nc(nc, pkg.env$node_count_var_name, node_count)
  return(invisible(nc))
}

put_part_node_count <- function(nc, part_node_count) {
  dim.def.nc(nc, pkg.env$part_dim_name, length(part_node_count))
  var.def.nc(nc, pkg.env$part_node_count_var_name, "NC_INT", pkg.env$part_dim_name)
  var.put.nc(nc, pkg.env$part_node_count_var_name, part_node_count)
  att.put.nc(nc, pkg.env$part_node_count_var_name, "long_name", "NC_CHAR", "count of nodes in each geometry part")
  att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$part_node_count_attr_name, "NC_CHAR", pkg.env$part_node_count_var_name)
  return(invisible(nc))
}

put_hole_node_count <- function(nc, part_type) {
  var.def.nc(nc, pkg.env$part_type_var_name, "NC_INT", pkg.env$part_dim_name)
  var.put.nc(nc, pkg.env$part_type_var_name, part_type)
  att.put.nc(nc, pkg.env$part_type_var_name, "long_name", "NC_CHAR", "type of each geometry part")
  att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$part_type_attr_name, "NC_CHAR", pkg.env$part_type_var_name)
  return(invisible(nc))
}

