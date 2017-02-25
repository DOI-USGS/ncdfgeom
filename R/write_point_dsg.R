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
#'@param feature_names \code{vector} of identifiers for features or stations.
#'@description
#'This creates a simple point data discrete sampling features NCDF file. Returns the created filename.
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@importFrom ncdf4 nc_create nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'
#'@export
write_point_dsg = function(nc_file, lats, lons, alts, data, data_units=rep('', ncol(data)), feature_names = NULL){
	
	n = length(lats)
	if(length(lats)!=n || length(lons)!=n || length(alts)!=n){
		stop('lats, lons, and alts must all be vectors of the same length')
	}
	
	nc_file <- write_instance_data(ncFile=nc_file, attData = data, instanceDimName = "obs", units = data_units)
	
	nc <- nc_open(nc_file, write = TRUE)
	
	if(!is.null(feature_names)) {
		if(length(lats)!=n || length(lons)!=n){
			stop('feature_names, lats, and lons must all be vectors of the same length')
		}
		strlen_dim = ncdim_def('name_strlen', '', 1:max(sapply(feature_names, nchar)), create_dimvar=FALSE)
		feature_var = ncvar_def('feature_name', '', dim=list(strlen_dim, nc$dim$obs), missval=NULL, prec='char', longname='Feature Names')
		nc <- ncvar_add(nc, feature_var)
	}
	
	#Setup our spatial and info
	lat_var = ncvar_def('lat', 'degrees_north', nc$dim$obs, -999, prec='double', longname = 'latitude of the observation')
	nc <- ncvar_add(nc, lat_var)
	lon_var = ncvar_def('lon', 'degrees_east', nc$dim$obs, -999, prec='double', longname = 'longitude of the observation')
	nc <- ncvar_add(nc, lon_var)
	alt_var = ncvar_def('alt', 'm', nc$dim$obs, -999, prec='double', longname='vertical distance above the surface')
	nc <- ncvar_add(nc, alt_var)
	
	nc_close(nc)
	
	nc <- nc_open(nc_file, write = TRUE)
	
	#add standard_names
	ncatt_put(nc, 'lat', 'standard_name', 'latitude')
	ncatt_put(nc, 'lon', 'standard_name', 'longitude')
	ncatt_put(nc, 'alt', 'standard_name', 'height')
	
	#use the same names for "standard names" and add coordinates as well
	for(data_name in names(data)){
		ncatt_put(nc, data_name, 'coordinates', 'lat lon alt')
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
	
	nc_close(nc)
	
	return(nc_file)
}
