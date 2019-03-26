#' @title Write NetCDF-CF point featuretype
#'
#' @param nc_file \code{character} file path to the nc file to be created.
#' @param lats \code{numeric} vector of latitudes 
#' @param lons \code{numeric} vector of longitudes
#' @param alts \code{numeric} vector of altitudes
#' @param times \code{POSIXct} vector of times, one per point, if length is one, 
#' will use the same time for each point. Must be of type \code{POSIXct} or an 
#' attempt to convert it will be made using \code{as.POSIXct(times)}.
#' @param data \code{data.frame} with each column corresponding to a observation. Column 
#' names are used as names in the NCDF file
#' @param data_units \code{character} vector of observation units. Length must be the same as number 
#' of columns in \code{data} parameter
#' @param feature_names \code{character} or \code{numeric} vector of identifiers for features or stations.
#' @param ... additional arguments to be passed on \code{nc_create}.
#'
#' @return \code{character} path of created file
#' 
#' @description
#' Creates a point feature type discrete sampling features NetCDF file. 
#' Returns the created filename. 
#' Can pass in netcdf creation options like force_v4 to pass on to nc_create().
#'
#'@references
#' \enumerate{
#'   \item \url{http://cfconventions.org/cf-conventions/cf-conventions.html#_features_and_feature_types}
#'   \item \url{http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html}
#'  }
#'
#'@importFrom ncdf4 nc_create nc_open nc_close ncvar_def ncvar_put ncatt_put ncdim_def ncvar_add
#'
#'@export
write_point_dsg = function(nc_file, lats, lons, alts, times, data, data_units=rep('', ncol(data)), feature_names = NULL, ...){
	
	n = length(lats)
	if(length(lats)!=n || length(lons)!=n || length(alts)!=n){
		stop('lats, lons, and alts must all be vectors of the same length')
	}
	
	if(!is.null(feature_names)) {
		if(length(lats)!=n || length(lons)!=n){
			stop('feature_names, lats, and lons must all be vectors of the same length')
		}
		data["feature_name"] <- feature_names
	}
	
	if(length(times)==1) {
		times<-rep(times, n)
	}
	
	if(!is(times, 'POSIXct')){
		times = as.POSIXct(times)
	}
	
	instance_dim_name <- "obs"
	
	nc_file <- write_attribute_data(nc_file=nc_file, attData = data, 
	                                instance_dim_name = instance_dim_name, 
	                                units = data_units, ...)
	
	nc <- open.nc(nc_file, write = TRUE)
	
	#Setup our spatial and info
	var.def.nc(nc, "lat", "NC_DOUBLE", instance_dim_name)
	att.put.nc(nc, "lat", "missing_value", "NC_DOUBLE", -999)
  att.put.nc(nc, "lat", "long_name", "NC_CHAR", "latitude of the observation")	
	att.put.nc(nc, "lat", "units", "NC_CHAR", 'degrees_north')
	
	var.def.nc(nc, "lon", "NC_DOUBLE", instance_dim_name)
	att.put.nc(nc, "lon", "missing_value", "NC_DOUBLE", -999)
	att.put.nc(nc, "lon", "long_name", "NC_CHAR", "longitude of the observation")	
	att.put.nc(nc, "lon", "units", "NC_CHAR", 'degrees_east')
	
	var.def.nc(nc, "alt", "NC_DOUBLE", instance_dim_name)
	att.put.nc(nc, "alt", "missing_value", "NC_DOUBLE", -999)
	att.put.nc(nc, "alt", "long_name", "NC_CHAR", "vertical distance above the surface")	
	att.put.nc(nc, "alt", "units", "NC_CHAR", 'm')
	
	var.def.nc(nc, "time", "NC_DOUBLE", instance_dim_name)
	att.put.nc(nc, "time", "long_name", "NC_CHAR", "time stamp")	
	att.put.nc(nc, "time", "units", "NC_CHAR", "days since 1970-01-01 00:00:00")
	
  close.nc(nc)
	nc <- open.nc(nc_file, write = TRUE)
	
	#add standard_names
	att.put.nc(nc, 'lat', 'standard_name', "NC_CHAR", 'latitude')
	att.put.nc(nc, 'lon', 'standard_name', "NC_CHAR", 'longitude')
	att.put.nc(nc, 'alt', 'standard_name', "NC_CHAR", 'height')
	att.put.nc(nc, 'time', 'standard_name', "NC_CHAR", 'time')
	
	#use the same names for "standard names" and add coordinates as well
	for(data_name in names(data)){
		att.put.nc(nc, data_name, 'coordinates', "NC_CHAR", 'lat lon alt time')
	}
	#some final stuff
	att.put.nc(nc, "NC_GLOBAL", 'featureType', "NC_CHAR", 'point')
	att.put.nc(nc, "NC_GLOBAL", 'Conventions', "NC_CHAR", 'CF-1.7')
	
	#Put data in NC file
	if(!is.null(feature_names)) {
		att.put.nc(nc, 'feature_name', 'long_name', "NC_CHAR", 'Feature Name')
	}
	
	var.put.nc(nc, 'lat', lats)
	var.put.nc(nc, 'lon', lons)
	var.put.nc(nc, 'alt', alts)
	var.put.nc(nc, 'time', as.numeric(times)/86400) #convert to days since 1970-01-01
	
	close.nc(nc)
	
	return(nc_file)
}
