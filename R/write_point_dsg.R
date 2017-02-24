#'@title Create point data NCDF file
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param lats Vector of latitudes 
#'@param lons Vector of longitudes
#'@param alts Vector of altitudes
#'@param data \code{data.frame} with each column corresponding to a observation. Column 
#'names are used as names in the NCDF file
#'@param data_units Character vector of observation units. Length must be the same as number 
#'of columns in \code{data} parameter
#'@param data_prec Precision of observation data in NCDF file. 
#'Valid options: 'short' 'integer' 'float' 'double' 'char'.
#'@description
#'This creates a simple point data discrete sampling features NCDF file. Returns the created filename.
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@importFrom ncdf4 nc_create nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'
#'@export
write_point_dsg = function(nc_file, lats, lons, alts, data, data_units=rep('', ncol(data)),
													 data_prec=rep('double', ncol(data)), feature_names = NULL){
	
	n = length(lats)
	if(length(lats)!=n || length(lons)!=n || length(alts)!=n){
		stop('lats, lons, and alts must all be vectors of the same length')
	}
	
	#Lay the foundation. This is a point featureType. Which has one dimension, "obs"
	obs_dim = ncdim_def('obs', '', 1:n, create_dimvar=FALSE)
	
	vars_to_write <- list()
	
	if(!is.null(feature_names)) {
		if(length(lats)!=n || length(lons)!=n){
			stop('feature_names, lats, and lons must all be vectors of the same length')
		}
		strlen_dim = ncdim_def('name_strlen', '', 1:max(sapply(feature_names, nchar)), create_dimvar=FALSE)
		feature_var = ncvar_def('feature_name', '', dim=list(strlen_dim, obs_dim), missval=NULL, prec='char', longname='Feature Names')
		vars_to_write <- c(vars_to_write, list(feature_var))
	}
	
	#Setup our spatial and info
	lat_var = ncvar_def('lat', 'degrees_north', obs_dim, -999, prec='double', longname = 'latitude of the observation')
	lon_var = ncvar_def('lon', 'degrees_east', obs_dim, -999, prec='double', longname = 'longitude of the observation')
	alt_var = ncvar_def('alt', 'm', obs_dim, -999, prec='double', longname='vertical distance above the surface')
	vars_to_write <- c(vars_to_write, list(lat_var, lon_var, alt_var))
	
	data_names = names(data)
	data_vars  = list()
	for(i in 1:ncol(data)){
		vars_to_write <- c(vars_to_write, list(ncvar_def(data_names[i], data_units[i], obs_dim, prec=data_prec[i], missval=-999)))
	}
	
	nc = nc_create(nc_file, vars = vars_to_write)
	
	#add standard_names
	ncatt_put(nc, 'lat', 'standard_name', 'latitude')
	ncatt_put(nc, 'lon', 'standard_name', 'longitude')
	ncatt_put(nc, 'alt', 'standard_name', 'height')
	
	#use the same names for "standard names" and add coordinates as well
	for(i in 1:ncol(data)){
		ncatt_put(nc, data_names[i], 'coordinates', 'lat lon alt')
	}
	#some final stuff
	ncatt_put(nc, 0,'featureType','point')
	ncatt_put(nc, 0,'Conventions','CF-1.7')
	ncatt_put(nc, 0,'cdm_data_type','Station')
	
	#Put data in NC file
		if(!is.null(feature_names)) {
		ncatt_put(nc, 'feature_name', 'cf_role', 'timeseries_id')
		ncvar_put(nc, 'feature_name', feature_names)
	}
	
	ncvar_put(nc, 'lat', lats)
	ncvar_put(nc, 'lon', lons)
	ncvar_put(nc, 'alt', alts)
	
	for(i in 1:ncol(data)){
		ncvar_put(nc, data_names[i], data[, i], count=n)
	}
	
	nc_close(nc)
	
	return(nc_file)
}
