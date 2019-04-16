#' @title Write sp or sf geometries and attributes to NetCDF-CF
#'
#' @param nc_file \code{character} file path to the nc file to be created.
#' @param instance_dim_name \code{character} Not required if adding geometry to a 
#' NetCDF-CF Discrete Sampling Geometries timeSeries file. For a new file, will 
#' use package default -- "instance" -- if not supplied.
#' @param geomData object of class \code{SpatialLines}, \code{SpatialPolygons}
#' or an sf \code{data.frame}. Note that three dimensional geometries are not supported.
#' @param variables \code{character} If a an existing netcdf files is provided, this 
#' list of variables that should be related to the geometries.
#'
#' @description
#' Creates a file with point, line or polygon instance data ready for the 
#' extended NetCDF-CF timeSeries featuretype format.
#' Will also add attributes if provided data has them.
#'
#' @references
#' http://cfconventions.org/index.html
#'
#' @importFrom sf st_set_geometry st_geometry
#' @export
#' 
#' @examples 
#'
#' hucPolygons <- sf::read_sf(system.file('extdata','example_huc_eta.json', package = 'ncdfgeom'))
#'
#' hucPolygons_nc <- ncdfgeom::write_geometry(nc_file=tempfile(), 
#'                                            geomData = hucPolygons)
#' ncdump <- system(paste("ncdump -h", hucPolygons_nc), intern = TRUE)
#' cat(ncdump ,sep = "\n")
#' 
write_geometry = function(nc_file, geomData = NULL, instance_dim_name = NULL, variables = list()) {

	geomData <- check_geomData(geomData)
	
  pointsMode <- FALSE

  type <- get_type(geomData)
    
  if(grepl("^POINT$", type)) {
    pointsMode <- TRUE
  } else if(grepl("MULTIPOINT", type)) {
  	stop("Multi point not supported yet.")
  } else if(!grepl("POLYGON", type) & !grepl("LINESTRING", type)) {
    stop("Did not find supported spatial data.")
  }
  
  attData <- st_set_geometry(geomData, NULL)
  geomData <- st_geometry(geomData)

  if(is.null(instance_dim_name)) {
    instance_dim_name <- pkg.env$instance_dim_name
  }

	if(ncol(attData) > 0) {
		itemp <- sapply(attData, is.factor)
		attData[itemp] <- lapply(attData[itemp], as.character)
		nc_file <- write_attribute_data(nc_file, attData, instance_dim_name)
		variables <- c(variables, names(attData))
	}

  nc_file <- write_geom_data(nc_file, geomData, instance_dim_name, variables = variables)

  return(nc_file)
}

#'@title Put geometry data in a NetCDF-CF File
#'
#'
#'@param nc_file A string file path to the nc file to be created. It must already have
#'an instance dimension.
#'@param geomData An object of class \code{SpatialLines}, \code{SpatialPolygons}
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
write_geom_data<-function(nc_file, geomData, instance_dim_name, variables = c()) {
  
  geomData <- check_geomData(geomData)
  
  type <- get_type(geomData)
  
  crs <- ncmeta::nc_prj_to_gridmapping(st_crs(geomData)$proj4string)
  crs <- setNames(crs$value, crs$name)
  
  if(length(crs) == 0) {
    crs <- list(grid_mapping_name = "latitude_longitude",
                semi_major_axis = 6378137,
                inverse_flattening = 298.257223563,
                longitude_of_prime_meridian = 0)
    warning("No CRS was found. Assuming WGS84 Lat Lon.")
  }
  
  node_dim_name <- pkg.env$node_dim_name
  
  linesMode <- FALSE
  pointsMode <- FALSE
  
  if(grepl("LINESTRING", type)) {
    linesMode<-TRUE
  }
  
  if(grepl("POINT", type)) {
    pointsMode <- TRUE
  }
  
  holes <- FALSE
  multis <- FALSE
  
  if(pointsMode) {

    geomData <- st_coordinates(geomData)
    
    node_count <- c(1:nrow(geomData))
    
    node_dim_name <- pkg.env$instance_dim_name
    
    xVals <- geomData[,1]
    yVals <- geomData[,2]
    
  } else {
    geomData <- st_coordinates(geomData)
    f <- ncol(geomData) # Feature designator is always last col.
    
    if(type == "MULTIPOLYGON") {
      p_id <- f - 1
      h_id <- f - 2
      
      hole_dif <- diff(geomData[, h_id])
      hole_dif[hole_dif < 0] <- 0
      part_dif <- diff(geomData[, p_id])
      
      geomData[, p_id] <- c(1, cumsum(hole_dif + part_dif) + 1)
      
    } else if(type == "POLYGON") {
      h_id <- f - 1
      p_id <- 0
    } else if(type == "MULTILINESTRING") {
      p_id <- f - 1
      h_id <- 0
    }  else if(type == "LINESTRING") {
      p_id <- 0
      h_id <- 0
    }
    
    nGeoms <- length(unique(geomData[, f]))
    
    nParts <- nGeoms
    if(p_id > 0) nParts <- length(unique(geomData[, p_id]))
    
    nCoords <- nrow(geomData)
    
    part_type <- rep(NA, nParts)
    part_node_count <- part_type
    node_count <- rep(NA, nGeoms)
    xVals <- rep(NA, nCoords)
    yVals <- xVals
    part <- 0
    coord <- 1
    
    for(geom in 1:nGeoms) {
      nCount <- 0
      gData <- geomData[geomData[, f] == geom, ]
      parts <- 1
      if(p_id > 0) parts <- length(unique(gData[, p_id]))
      if(p_id == 0 & h_id > 0) parts <- length(unique(gData[, h_id]))
      for(gPart in 1:parts) {
        part <- part + 1
        if(gPart > 1) {
          if(!linesMode && 
             any(gData[gData[, p_id] == gPart, h_id] > 1) || 
             (p_id == 0 & any(gData[gData[, h_id] == gPart] > 1))) {
            part_type[part] <- pkg.env$hole_val
            holes <- TRUE
          } else {
            part_type[part] <- pkg.env$multi_val
            multis <- TRUE
          }
        } else {
          part_type[part] <-pkg.env$multi_val
        }
        
        if(p_id == 0 & h_id == 0) {
          coords <- gData[, c(1,2)]
        } else if(h_id == 0 & p_id > 0) {
          coords <- gData[gData[, p_id] == gPart, c(1,2)]
        } else if(p_id == 0 & h_id > 0) {
          coords <- gData[gData[, h_id] == gPart, c(1,2)]
        } else {
          coords <- gData[gData[, p_id] == gPart, c(1,2)]
        }
        
        pCount <- nrow(coords)
        nCount <- nCount + pCount
        part_node_count[part] <-  pCount
        if(linesMode) {
          xVals[coord:(coord+nrow(coords)-1)] <- coords[,1]
          yVals[coord:(coord+nrow(coords)-1)] <- coords[,2]
        } else {
          xVals[coord:(coord+nrow(coords)-1)]<-coords[1:nrow(coords), 1]
          yVals[coord:(coord+nrow(coords)-1)]<-coords[1:nrow(coords), 2]
        }
        coord <- coord + nrow(coords)
      }
      node_count[geom] <- nCount
    }
  }
  
  if(file.exists(nc_file)) {
    nc <- open.nc(nc_file, write = TRUE)
  } else {
    nc <- create.nc(nc_file, large = TRUE)
  }
  
  try(dim.def.nc(nc, node_dim_name, length(xVals), unlim = FALSE), silent = TRUE)
  
  var.def.nc(nc, pkg.env$x_nodes, "NC_DOUBLE", node_dim_name)
  var.def.nc(nc, pkg.env$y_nodes, "NC_DOUBLE", node_dim_name)
  
  att.put.nc(nc, pkg.env$x_nodes, "units", "NC_CHAR", "degrees_east")
  att.put.nc(nc, pkg.env$y_nodes, "units", "NC_CHAR", "degrees_north")

  close.nc(nc)
  nc <- open.nc(nc_file, write = TRUE)
  
  var.put.nc(nc, pkg.env$x_nodes, xVals)
  var.put.nc(nc, pkg.env$y_nodes, yVals)
  att.put.nc(nc, pkg.env$x_nodes, 'standard_name', "NC_CHAR", 'longitude')
  att.put.nc(nc, pkg.env$y_nodes, 'standard_name', "NC_CHAR", 'latitude')
  att.put.nc(nc, pkg.env$x_nodes, "axis", "NC_CHAR", "X")
  att.put.nc(nc, pkg.env$y_nodes, "axis", "NC_CHAR", "Y")
  
  geom_container <- var.def.nc(nc, pkg.env$geom_container_var_name, "NC_INT", NA)
  
  close.nc(nc)
  nc <- open.nc(nc_file,write = TRUE)
  
  if(pointsMode) {
    att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name, "NC_CHAR", 'point')
  } else if(linesMode) {
    att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name, "NC_CHAR", 'line')
  } else {
    att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name, "NC_CHAR", 'polygon')
  }
  if(!(pointsMode && !multis)) {
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
  }
  
  att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$node_coordinates, "NC_CHAR", paste(pkg.env$x_nodes, pkg.env$y_nodes))
  
  if(length(crs) > 0) {
    att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$crs, "NC_CHAR", pkg.env$crs_var_name)
    
    for(var in variables) {
      att.put.nc(nc, var, pkg.env$crs, "NC_CHAR", pkg.env$crs_var_name)
    }
    
    crs_var <- var.def.nc(nc, pkg.env$crs_var_name, vartype = "NC_INT", dimensions = NA)

    close.nc(nc)
    nc <- open.nc(nc_file,write = TRUE)
    
    
    for(crs_att in names(crs)) att.put.nc(nc, pkg.env$crs_var_name, crs_att, pkg.env$nc_types[class(crs[crs_att][[1]])][[1]], crs[crs_att][[1]])
  }
  
  if(!pointsMode && (multis || holes)) {
    dim.def.nc(nc, pkg.env$part_dim_name, length(part_node_count))
    var.def.nc(nc, pkg.env$part_node_count_var_name, "NC_INT", pkg.env$part_dim_name)
    var.put.nc(nc, pkg.env$part_node_count_var_name, part_node_count)
    att.put.nc(nc, pkg.env$part_node_count_var_name, "long_name", "NC_CHAR", "count of nodes in each geometry part")
    att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$part_node_count_attr_name, "NC_CHAR", pkg.env$part_node_count_var_name)
    
    if(holes) {
      var.def.nc(nc, pkg.env$part_type_var_name, "NC_INT", pkg.env$part_dim_name)
      var.put.nc(nc, pkg.env$part_type_var_name, part_type)
      att.put.nc(nc, pkg.env$part_type_var_name, "long_name", "NC_CHAR", "type of each geometry part")
      att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$part_type_attr_name, "NC_CHAR", pkg.env$part_type_var_name)
    }
  }
  
  att.put.nc(nc, "NC_GLOBAL", "Conventions", "NC_CHAR", pkg.env$cf_version)
  
  for(var in variables) {
    att.put.nc(nc, var, pkg.env$geometry_container_att_name, "NC_CHAR", pkg.env$geom_container_var_name)
  }
  
  close.nc(nc)
  return(nc_file)
}

get_type <- function(geomData) {
  type <- unique(st_geometry_type(geomData))
  
  if(length(type) > 1) stop("Found multiple geometry types, only one is supported.")
  
  return(type)
}