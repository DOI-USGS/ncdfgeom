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
	
	nc_file <- write_attribute_data(nc_file=nc_file, attData = data, instance_dim_name = "obs", units = data_units, ...)
	
	nc <- nc_open(nc_file, write = TRUE)
	
	#Setup our spatial and info
	lat_var = ncvar_def('lat', 'degrees_north', nc$dim$obs, -999, prec='double', longname = 'latitude of the observation')
	nc <- ncvar_add(nc, lat_var)
	lon_var = ncvar_def('lon', 'degrees_east', nc$dim$obs, -999, prec='double', longname = 'longitude of the observation')
	nc <- ncvar_add(nc, lon_var)
	alt_var = ncvar_def('alt', 'm', nc$dim$obs, -999, prec='double', longname='vertical distance above the surface')
	nc <- ncvar_add(nc, alt_var)
	time_var = ncvar_def('time', 'days since 1970-01-01 00:00:00', nc$dim$obs, -999, prec='integer', longname = 'time stamp')
	nc <- ncvar_add(nc, time_var)
	
	nc_close(nc)
	
	nc <- nc_open(nc_file, write = TRUE)
	
	#add standard_names
	ncatt_put(nc, 'lat', 'standard_name', 'latitude')
	ncatt_put(nc, 'lon', 'standard_name', 'longitude')
	ncatt_put(nc, 'alt', 'standard_name', 'height')
	ncatt_put(nc, 'time', 'standard_name', 'time')
	
	#use the same names for "standard names" and add coordinates as well
	for(data_name in names(data)){
		ncatt_put(nc, data_name, 'coordinates', 'lat lon alt time')
	}
	#some final stuff
	ncatt_put(nc, 0,'featureType','point')
	ncatt_put(nc, 0,'Conventions','CF-1.7')
	
	#Put data in NC file
	if(!is.null(feature_names)) {
		ncatt_put(nc, 'feature_name', 'long_name', 'Feature Name')
	}
	
	ncvar_put(nc, 'lat', lats)
	ncvar_put(nc, 'lon', lons)
	ncvar_put(nc, 'alt', alts)
	ncvar_put(nc, 'time', as.numeric(times)/86400) #convert to days since 1970-01-01
	
	nc_close(nc)
	
	return(nc_file)
}
