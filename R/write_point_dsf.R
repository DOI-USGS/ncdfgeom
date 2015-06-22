#'@title Create point data NCDF file
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param times A vector of times. Must be of type \code{\link{POSIXct}} or an attempt to 
#'convert it will be made using \code{\link{as.POSIXct(times)}}.
#'@param lats Vector of latitudes 
#'@param lons Vector of longitudes
#'@param alts Vector of altitudes
#'@param data \code{data.frame} with each column corresponding to a observation. Column 
#'names are used as names in the NCDF file
#'@param data_units Character vector of observation units. Length must be the same as number 
#'of columns in \code{data} parameter
#'@param data_prec Precision of observation data in NCDF file. 
#'Valid options: 'short' 'integer' 'float' 'double' 'char'.
#'
#'
#'@description
#'This creates a simple point data discrete sampling features NCDF file
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@import ncdf
#'
#'@export
write_point_dsf = function(nc_file, times, lats, lons, alts, data, data_units=rep('', ncol(data)),
													 data_prec=rep('double', ncol(data))){
	
	#building this with what I think is the minium required as shown here:
	# http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/cf-conventions.html#idm43165559776
	
	if(!is(times, 'POSIXct')){
		times = as.POSIXct(times)
	}
	
	n = length(times)
	if(length(lats)!=n || length(lons)!=n || length(alts)!=n){
		stop('times, lats, lons, and alts must all be vectors of the same length')
	}
	
	
	#Lay the foundation. This is a point featureType. Which has one dimension, "obs"
	obs_dim = dim.def.ncdf('obs', '', 1:n, unlim = TRUE, create_dimvar=FALSE)
	
	#Setup our spatial and time info
	time_var =var.def.ncdf('time','days since 1970-01-01 00:00:00', obs_dim, -999, prec='double', longname='time of measurement')
	lat_var = var.def.ncdf('lat', 'degrees_north', obs_dim, -999, prec='double', longname = 'latitude of the observation')
	lon_var = var.def.ncdf('lon', 'degrees_east', obs_dim, -999, prec='double', longname = 'longitude of the observation')
	alt_var = var.def.ncdf('alt', 'm', obs_dim, -999, prec='double', longname='vertical distance above the surface')
	
	data_names = names(data)
	data_vars  = list()
	for(i in 1:ncol(data)){
		data_vars[[i]] = var.def.ncdf(data_names[i], data_units[i], obs_dim, prec=data_prec[i], missval=-999)
	}
	
	nc_file = create.ncdf(nc_file, vars = c(list(lat_var, lon_var, alt_var, time_var), data_vars))
	
	#add standard_names
	att.put.ncdf(nc_file, 'lat', 'standard_name', 'latitude')
	att.put.ncdf(nc_file, 'time', 'standard_name', 'time')
	att.put.ncdf(nc_file, 'lon', 'standard_name', 'longitude')
	att.put.ncdf(nc_file, 'alt', 'standard_name', 'height')
	
	#use the same names for "standard names" and add coordinates as well
	for(i in 1:ncol(data)){
		att.put.ncdf(nc_file, data_names[i], 'standard_name', data_names[i])
		att.put.ncdf(nc_file, data_names[i], 'coordinates', 'time lat lon alt')
	}
	#some final stuff
	att.put.ncdf(nc_file, 0,'featureType','point')
	
	#Put data in NC file
	put.var.ncdf(nc_file, 'time', as.numeric(times)/86400, count=n) #convert to days since 1970-01-01
	put.var.ncdf(nc_file, 'lat', lats, count=n)
	put.var.ncdf(nc_file, 'lon', lons, count=n)
	put.var.ncdf(nc_file, 'alt', alts, count=n)
	
	for(i in 1:ncol(data)){
		put.var.ncdf(nc_file, data_names[i], data[, i], count=n)
	}
	
	close.ncdf(nc_file)
}
