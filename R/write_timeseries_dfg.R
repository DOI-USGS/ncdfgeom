#'@title Create timeseries NCDF file
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param times A vector of times. Must be of type \code{\link{POSIXct}} or an attempt to 
#'convert it will be made using \code{\link{as.POSIXct(times)}}.
#'@param lats Vector of latitudes 
#'@param lons Vector of longitudes
#'@param data \code{data.frame} with each column corresponding to a observation. Rows correspond to 
#'samples at different times. nrow must be the same length as times. 
#'Column names are used as names in the NCDF file.
#'@param data_unit Character vector of observation units. Length must be the same as number 
#'of columns in \code{data} parameter
#'@param data_prec Precision of observation data in NCDF file. 
#'Valid options: 'short' 'integer' 'float' 'double' 'char'.
#'@param attributes An optional list of attributes that will be added at the global level. 
#'See details for useful attributes.
#'
#'@description
#'This creates a timeseries discrete sampling features NCDF file
#'
#'@details
#'title = "title"
#'abstract = "history"
#'provider site = "institution"
#'provider name ="source"
#'description = "description"
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@import ncdf
#'@importFrom methods is
#'
#'@export
write_timeseries_dsg = function(nc_file, station_names, lats, lons, times, data, data_unit='',
													 data_prec='double', attributes=list()){
	
	#building this with what I think is the minium required as shown here:
	# http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/cf-conventions.html#time-series-data
	
	if(!is(times, 'POSIXct')){
		times = as.POSIXct(times)
	}
	
	n = length(station_names)
	if(length(lats)!=n || length(lons)!=n){
		stop('station_names, lats, and lons must all be vectors of the same length')
	}
	
	if(ncol(data)!=n){
		stop('Only one variable is currently supported, ncol(data) must equal the number of stations')
	}
	
	nt = length(times)
	if(nrow(data) != nt){
		stop('The length of times must match the number of rows in data')
	}
	
	
	#Lay the foundation. This is a point featureType. Which has one dimension, "obs"
	#obs_dim = dim.def.ncdf('obs', '', 1:n, unlim = TRUE, create_dimvar=FALSE)
	station_dim = dim.def.ncdf('station', '', 1:n, create_dimvar=FALSE)
	time_dim = dim.def.ncdf('time', '', 1:nt, unlim=FALSE, create_dimvar=FALSE)
	strlen_dim = dim.def.ncdf('name_strlen', '', 1:max(sapply(station_names, nchar)), create_dimvar=FALSE)
	
	#Setup our spatial and time info
	station_var = var.def.ncdf('station_name', '', list(strlen_dim, station_dim), missval='', prec='char', longname='Station Names')
	time_var 		= var.def.ncdf('time','days since 1970-01-01 00:00:00', time_dim, -999, prec='double', longname='time of measurement')
	lat_var 		= var.def.ncdf('lat', 'degrees_north', station_dim, -999, prec='double', longname = 'latitude of the observation')
	lon_var 		= var.def.ncdf('lon', 'degrees_east', station_dim, -999, prec='double', longname = 'longitude of the observation')
	#alt_var 		= var.def.ncdf('alt', 'm', station_dim, -999, prec='double', longname='vertical distance above the surface')
	
	data_vars = list()
	data_name = names(data)[1]
	data_vars[[1]] = var.def.ncdf(data_name, data_unit, list(time_dim, station_dim), prec=data_prec, missval=-999)
		
	#nc_file = create.ncdf(nc_file, vars = c(list(lat_var, lon_var, alt_var, time_var, station_var), data_vars))
	nc_file = create.ncdf(nc_file, vars = c(list(lat_var, lon_var, time_var, station_var), data_vars))
	
	#add standard_names
	att.put.ncdf(nc_file, 'lat', 'standard_name', 'latitude')
	att.put.ncdf(nc_file, 'time', 'standard_name', 'time')
	att.put.ncdf(nc_file, 'lon', 'standard_name', 'longitude')
	#att.put.ncdf(nc_file, 'alt', 'standard_name', 'height')
	att.put.ncdf(nc_file, 'station_name', 'cf_role', 'timeseries_id')
	
	#use the same names for "standard names" and add coordinates as well
	att.put.ncdf(nc_file, data_name, 'standard_name', data_name)
	att.put.ncdf(nc_file, data_name, 'coordinates', 'time lat lon')
	
	#some final stuff
	att.put.ncdf(nc_file, 0,'Conventions','CF-1.7')
	att.put.ncdf(nc_file, 0,'featureType','timeSeries')
	att.put.ncdf(nc_file, 0,'cdm_data_type','Station')
	att.put.ncdf(nc_file, 0,'standard_name_vocabulary','CF-1.7')
	
	#Put data in NC file
	put.var.ncdf(nc_file, time_var, as.numeric(times)/86400, count=nt) #convert to days since 1970-01-01
	put.var.ncdf(nc_file, lat_var, lats, count=n)
	put.var.ncdf(nc_file, lon_var, lons, count=n)
	#put.var.ncdf(nc_file, alt_var, alts, count=n)
	put.var.ncdf(nc_file, station_var, station_names, count=c(-1,n))
	
	put.var.ncdf(nc_file, data_name, as.vector((as.matrix(data))), start=c(1,1), count=c(nt, n))
	
	#Add the optional global attributes
	for(i in 1:length(attributes)){
		att.put.ncdf(nc_file, 0, names(attributes[i]), attributes[[i]])
	}
	
	close.ncdf(nc_file)
}
