#' @title Write sp or sf geometries and attributes to NetCDF-CF
#'
#' @param nc_file \code{character} file path to the nc file to be created.
#' @param instance_dim_name \code{character} Not required if adding geometry to a 
#' NetCDF-CF Discrete Sampling Geometries timeSeries file. For a new file, will 
#' use package default -- "instance" -- if not supplied.
#' @param geomData object of class \code{SpatialLines}, \code{SpatialPolygons}
#' or an sf \code{data.frame}. Note that three dimensional geometries are not supported.
#' @param lats Vector of latitudes
#' @param lons Vector of longitudes
#' @param variables \code{character} If a an existing netcdf files is provided, this 
#' list of variables that should be related to the geometries.
#'
#' @description
#' Creates a file with point, line or polygon instance data ready for the 
#' extended NetCDF-CF timeSeries featuretype format.
#' Will also add attributes if provided data has them.
#'
#' @references
#' https://github.com/twhiteaker/netCDF-CF-simple-geometry
#'
#' @importFrom sp SpatialLinesDataFrame polygons SpatialPoints
#'
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
write_geometry = function(nc_file, geomData = NULL, instance_dim_name = NULL, lats = NULL, lons = NULL, variables = list()){

	geomData <- check_geomData(geomData)
	
  pointsMode <- FALSE

  if(class(geomData) == "SpatialPolygonsDataFrame") {
    attData<-geomData@data
    geomData<-polygons(geomData)
  } else if(class(geomData) == "SpatialLinesDataFrame") {
    attData<-geomData@data
  } else if(class(geomData) == "SpatialPolygons") {
    geomData<-polygons(geomData)
  } else if(class(geomData) == "SpatialLines") {
    geomData<-geomData
  } else if(class(geomData) == "SpatialPoints") {
    pointsMode<-TRUE
  } else if(class(geomData) == "SpatialPointsDataFrame") {
    pointsMode<-TRUE
    attData<-geomData@data
  } else if(class(geomData) == "SpatialMultiPointsDataFrame" | class(geomData) == "SpatialMultiPoints") {
  	stop("Multi point not supported yet.")
  } else if(!is.null(lats)) {
    pointsMode<-TRUE
    geomData <- SpatialPoints(as.data.frame(list(x=lons, y=lats)),proj4string = CRS("+proj=longlat +datum=WGS84"))
  } else {
    stop("Did not find supported spatial data.")
  }

  if(is.null(instance_dim_name)) {
    instance_dim_name <- pkg.env$instance_dim_name
  }

  if(exists("attData")) {
  	if(ncol(attData) > 0) {
  		itemp <- sapply(attData, is.factor)
  		attData[itemp] <- lapply(attData[itemp], as.character)
  		nc_file <- write_attribute_data(nc_file, attData, instance_dim_name)
  		variables <- c(variables, names(attData))
  	}
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
#' @noRd
write_geom_data<-function(nc_file, geomData, instance_dim_name, variables = c()) {
  
  geomData <- check_geomData(geomData)
  
  node_dim_name <- pkg.env$node_dim_name
  
  linesMode <- FALSE
  pointsMode <- FALSE
  
  if(class(geomData) == "SpatialLines" || class(geomData) == "SpatialLinesDataFrame") {
    linesMode<-TRUE
  }
  
  if(class(geomData) == "SpatialPoints" || class(geomData) == "SpatialPointsDataFrame") {
    pointsMode <- TRUE
    xCoords<-geomData@coords[,1]
    yCoords<-geomData@coords[,2]
  }
  
  holes <- FALSE
  multis <- FALSE
  
  if(pointsMode) {
    ids <- attributes(geomData@coords)$dimnames[[1]]
    uIds <- unique(ids)
    node_count <- c(1:length(uIds))
    if(length(ids) != length(uIds)) {
      multis <- TRUE
      for(i in 1:length(uIds)) {
        node_count[i] <- length(which(ids == uIds[i]))
      }
    } else {
      node_dim_name <- pkg.env$instance_dim_name
    }
    xVals <- geomData@coords[,1]
    yVals <- geomData@coords[,2]
  } else {
    nCoords <- 0
    nParts <- 0
    nGeoms <- length(geomData)
    for(geom in 1:nGeoms) {
      if(linesMode) { gData <- geomData@lines[[geom]]@Lines
      } else { gData <- geomData@polygons[[geom]]@Polygons }
      nParts <- nParts + length(gData)
      for(part in 1:length(gData)) {
        nCoords <- nCoords + length(gData[[part]]@coords[,1])
      }
    }
    part_type <- rep(NA,nParts)
    part_node_count <- part_type
    node_count <- rep(NA, nGeoms)
    xVals <- rep(NA, nCoords)
    yVals <- xVals
    part <- 0
    coord <- 1
    for(geom in 1:nGeoms) {
      nCount <- 0
      if(linesMode) { gData <- geomData@lines[[geom]]@Lines
      } else { gData <- geomData@polygons[[geom]]@Polygons }
      for(gPart in 1:length(gData)) {
        part <- part + 1
        if(gPart > 1) {
          if(!linesMode && gData[[gPart]]@hole) {
            part_type[part] <- pkg.env$hole_val
            holes <- TRUE
          } else {
            part_type[part] <- pkg.env$multi_val
            multis <- TRUE
          }
        } else {
          part_type[part] <-pkg.env$multi_val
        }
        coords<-gData[[gPart]]@coords
        pCount <- length(coords[,1])
        nCount <- nCount + pCount
        part_node_count[part] <-  pCount
        if(linesMode) {
          xVals[coord:(coord+length(coords[,1])-1)] <- coords[,1]
          yVals[coord:(coord+length(coords[,2])-1)] <- coords[,2]
        } else {
          xVals[coord:(coord+length(coords[,1])-1)]<-coords[nrow(coords):1,1]
          yVals[coord:(coord+length(coords[,2])-1)]<-coords[nrow(coords):1,2]
        }
        coord <- coord + length(coords[,1])
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
  
  crs <- ncmeta::nc_prj_to_gridmapping(geomData@proj4string)
  crs <- setNames(crs$value, crs$name)
  
  if(length(crs) == 0) {
    crs <- list(grid_mapping_name = "latitude_longitude",
                semi_major_axis = 6378137,
                inverse_flattening = 298.257223563,
                longitude_of_prime_meridian = 0)
    warning("No CRS was found. Assuming WGS84 Lat Lon.")
  }
  
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

