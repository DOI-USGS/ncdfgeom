#'@title Convert sp objects to NetCDF
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param instance_names A character vector of names for geometries.
#'If NULL, integers are used. If the geomData has a data frame, this is not used.
#'@param instance_dim_name If the file provided already has an instance dimension,
#'it needs to be provided as a character string otherwise a new instance dim may be created.
#'@param geomData An object of class \code{SpatialLines}, \code{SpatialPolygons}
#'or their sf with WGS84 lon in the x coordinate and lat in the y coordinate.
#'Note that three dimensional geometries are not supported.
#'@param lats Vector of WGS84 latitudes
#'@param lons Vector of WGS84 longitudes
#'@param variables If a an existing netcdf files is provided, this list of strings is used
#'to add the geometry container attribute to the named existing variables.
#'
#'@description
#'Creates a file with point, line or polygon instance data ready for the extended NetCDF-CF timeSeries featuretype format.
#'Will also add attributes if a sp dataframe object is passed in.
#'
#'@references
#'https://github.com/twhiteaker/netCDF-CF-simple-geometry
#'
#'@importFrom ncdf4 nc_open ncvar_add nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'@importFrom sp SpatialLinesDataFrame polygons SpatialPoints
#'
#'@export
ToNCDFSG = function(nc_file, geomData = NULL, instance_names = NULL, instance_dim_name = NULL, lats = NULL, lons = NULL, variables = list()){

	geomData <- check_geomData(geomData)
	
  pointsMode <- FALSE

  if(is.null(instance_names) && !is.null(geomData)) {
    if(class(geomData)=="SpatialPoints" || class(geomData)=="SpatialPointsDataFrame") {
      instance_names <- as.character(unique(attributes(geomData@coords)$dimnames[[1]]))
    } else {
      instance_names <- as.character(c(1:length(geomData)))
    }
  }

  if(class(geomData) == "SpatialPolygonsDataFrame") {
    attData<-geomData@data
    geomData<-polygons(geomData)
  } else if(class(geomData) == "SpatialLinesDataFrame") {
    attData<-geomData@data
  } else if(class(geomData) == "SpatialPolygons") {
    geomData<-polygons(geomData)
  } else if(class(geomData) == "SpatialLines") {
    geomData<-SpatialLinesDataFrame(geomData,data=as.data.frame(instance_names,stringsAsFactors = FALSE))
  } else if(class(geomData) == "SpatialPoints") {
    pointsMode<-TRUE
  } else if(class(geomData) == "SpatialPointsDataFrame") {
    pointsMode<-TRUE
    attData<-geomData@data
  } else if(!is.null(lats)) {
    pointsMode<-TRUE
    geomData <- SpatialPoints(as.data.frame(list(x=lons, y=lats)),proj4string = CRS("+proj=longlat +datum=WGS84"))
    if(is.null(instance_names)) {
      instance_names<-as.character(c(1:length(lats)))
    }
  } else {
    stop("Did not find supported spatial data.")
  }

  if(!pointsMode && !is.null(geomData)) {
    if(length(instance_names)!=length(geomData)) stop('instance_names must be same length as data')
  }

  if(is.null(instance_dim_name)) {
    instance_dim_name <- pkg.env$instance_dim_name
  }

  if(exists("attData")) {
    itemp <- sapply(attData, is.factor)
    attData[itemp] <- lapply(attData[itemp], as.character)
    nc_file <- write_instance_data(nc_file, attData, instance_dim_name)
    variables <- c(variables, names(attData))
  }

  nc_file <- addGeomData(nc_file, geomData, instance_dim_name, variables = variables)

  return(nc_file)
}


