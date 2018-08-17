#'@title Create timeseries NCDF file
#'
#'@param nc_file A string file path to the nc file to be created.
#'@param instance_names A vector of names for each instance 
#'(e.g. station or geometry) to be added to the file.
#'@param times vector of times. Must be of type \code{POSIXct} or an attempt to 
#'convert it will be made using \code{as.POSIXct(times)}.
#'@param lats numeric vector of latitudes 
#'@param lons numeric vector of longitudes
#'@param data \code{data.frame} with each column corresponding to an instance. Rows correspond to 
#'time steps. nrow must be the same length as times. Column names must match instance names.
#'@param alts Vector of altitudes (m above sea level) (Optional)
#'@param data_unit character vector of observation units. Length must be the same as number 
#'of columns in \code{data} parameter.
#'@param data_prec character Precision of observation data in NCDF file. 
#'Valid options: 'short' 'integer' 'float' 'double' 'char'.
#'@param data_metadata list A named list of strings: list(name='ShortVarName', long_name='A Long Name')
#'@param attributes list An optional list of attributes that will be added at the global level. 
#'See details for useful attributes.
#'@param add_to_existing boolean If TRUE and the file already exists, 
#'variables will be added to the existing file. See details for more.
#'
#'@description
#'This function creates a timeseries discrete sampling geometry NetCDF file.
#'It uses the orthogonal array encoding to write one \code{data.frame} per
#'function call. This encoding is best suited to data with the same number of
#'timesteps per instance (e.g. geometry or station).
#'
#'@details
#'Suggested Global Variables:
#'c(title = "title", 
#'abstract = "history", 
#'provider site = "institution", 
#'provider name ="source", 
#'description = "description")
#'
#'Note regarding add_to_existing:
#'add_to_existing = TRUE should only be used to add variables to an existing 
#'NetCDF discrete sampling geometry file. All other inputs should be the 
#'same as are already in the file. If the functions is called with 
#'add_to_existing=FALSE (the default), it will overwrite an existing file 
#'with the same name. The expected usage is to call this function repeatedly 
#'only changing the data, data_unit, data_prec and data_metadata inputs.
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'http://cfconventions.org/cf-conventions/cf-conventions.html#_orthogonal_multidimensional_array_representation
#'http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/cf-conventions.html#time-series-data
#'
#'@importFrom ncdf4 nc_create nc_close ncvar_def ncvar_put ncatt_put ncdim_def
#'@importFrom methods is
#'
#'@export
write_timeseries_dsg = function(nc_file, instance_names, lats, lons, times, data, alts=NA, data_unit='',
																data_prec='double',data_metadata=list(name='data',long_name='unnamed data'),
																attributes=list(),add_to_existing=FALSE){
	
	if(add_to_existing && !file.exists(nc_file)) add_to_existing=FALSE
	
	if(!is(times, 'POSIXct')){
		times = as.POSIXct(times)
	}
	
	n = length(instance_names)
	if(length(lats)!=n || length(lons)!=n){
		stop('instance_names, lats, and lons must all be vectors of the same length')
	}
	
	if(!is.na(alts[1]) && length(alts)!=n){
		stop('instance_names and alts must all be vectors of the same length')
	}
	
	if(ncol(data)!=n){
		stop('Only one variable is currently supported, ncol(data) must equal the number of stations')
	}
	
	nt = length(times)
	if(nrow(data) != nt){
		stop('The length of times must match the number of rows in data')
	}
	
	if(!all(sapply(data,typeof) %in% typeof(data[names(data)[1]][[1]]))) {
		stop('All the collumns in the input dataframe must be of the same type.')
	}
	
	# Set up data_name var.
	data_name = data_metadata[['name']]
	
	if(add_to_existing) {
		# Open existing file.
		nc<-nc_open(nc_file, write = TRUE)
		data_vars = list()
		data_vars[[1]] = ncvar_def(data_name, data_unit, dim=list(nc$dim$time, nc$dim$station), prec=data_prec, 
															 longname=data_metadata[['long_name']], missval=-999)
		ncvar_add(nc, data_vars[[1]])
		nc_close(nc)
		nc<-nc_open(nc_file, write = TRUE)
		putDataInNC(nc,nt,n,data_name,data)
		nc_close(nc)
	} else {
		station_dim = ncdim_def(pkg.env$instance_dim_name, '', 1:n, create_dimvar=FALSE)
		time_dim = ncdim_def(pkg.env$time_dim_name, '', 1:nt, unlim=FALSE, create_dimvar=FALSE)
		strlen_dim = ncdim_def(pkg.env$str_len_dim, '', 1:max(sapply(instance_names, nchar)), create_dimvar=FALSE)
		
		#Setup our spatial and time info
		station_var = ncvar_def(pkg.env$dsg_timeseries_id, '', dim=list(strlen_dim, station_dim), missval=NULL, prec='char', longname='Station Names')
		time_var 		= ncvar_def(pkg.env$time_var_name,'days since 1970-01-01 00:00:00', dim=time_dim, -999, prec='double', longname='time of measurement')
		lat_var 		= ncvar_def(pkg.env$lat_coord_var_name, 'degrees_north', dim=station_dim, -999, prec='double', longname = 'latitude of the observation')
		lon_var 		= ncvar_def(pkg.env$lon_coord_var_name, 'degrees_east', dim=station_dim, -999, prec='double', longname = 'longitude of the observation')
		
		if(!is.na(alts[1])){
			alt_var = ncvar_def(pkg.env$alt_coord_var_name, 'm', dim=station_dim, missval=-999, prec='double', longname='vertical distance above the surface')
		}
		
		data_vars = list()
		data_vars[[1]] = ncvar_def(data_name, data_unit, dim=list(time_dim, station_dim), prec=data_prec, 
															 longname=data_metadata[['long_name']], missval=-999)
		
		if(!is.na(alts[1])){
			nc = nc_create(nc_file, vars = c(list(lat_var, lon_var, time_var, alt_var, station_var), data_vars))
		} else {
			nc = nc_create(nc_file, vars = c(list(lat_var, lon_var, time_var, station_var), data_vars))
		}
		
		nc_close(nc)
		nc<-nc_open(nc_file, write = TRUE)
		#add standard_names
		ncatt_put(nc, pkg.env$lat_coord_var_name, 'standard_name', pkg.env$lat_coord_var_standard_name)
		ncatt_put(nc, pkg.env$time_var_name, 'standard_name', pkg.env$time_var_standard_name)
		ncatt_put(nc, pkg.env$lon_coord_var_name, 'standard_name', pkg.env$lon_coord_var_standard_name)
		
		if(!is.na(alts[1])){
			ncatt_put(nc, pkg.env$alt_coord_var_name, 'standard_name', pkg.env$alt_coord_var_standard_name)
		}
		
		ncatt_put(nc, pkg.env$dsg_timeseries_id, 'cf_role', pkg.env$timeseries_id_cf_role)
		
		#Important Global Variables
		ncatt_put(nc, 0,'Conventions',pkg.env$cf_version)
		ncatt_put(nc, 0,'featureType','timeSeries')
		ncatt_put(nc, 0,'cdm_data_type','Station')
		ncatt_put(nc, 0,'standard_name_vocabulary',pkg.env$cf_version)
		
		#Add the optional global attributes
		if(length(attributes)>0){
			for(i in 1:length(attributes)){
				ncatt_put(nc, 0, names(attributes)[i], attributes[[i]])
			}
		}
		
		#Put data in NC file
		ncvar_put(nc, time_var, as.numeric(times)/86400, count=nt) #convert to days since 1970-01-01
		ncvar_put(nc, lat_var, lats, count=n)
		ncvar_put(nc, lon_var, lons, count=n)
		
		if(!is.na(alts[1])){
			ncvar_put(nc, alt_var, alts, count=n)
		}
		ncvar_put(nc, station_var, instance_names, count=c(-1,n))
		
		putDataInNC(nc,nt,n,data_name,data)
		
		nc_close(nc) 
		
		return(nc_file)
	}
}

putDataInNC<-function(nc,nt,n,data_name,data,alts=NA) {
	#Add coordinates
	if(!is.na(alts[1])){
		ncatt_put(nc, data_name, 'coordinates', paste(pkg.env$time_var_name,
		                                              pkg.env$lat_coord_var_name,
		                                              pkg.env$lon_coord_var_name,
		                                              pkg.env$alt_coord_var_name))
	} else {
		ncatt_put(nc, data_name, 'coordinates', paste(pkg.env$time_var_name,
		                                              pkg.env$lat_coord_var_name,
		                                              pkg.env$lon_coord_var_name))
	}
	if ( nt * n < 100000 ) {
		ncvar_put(nc, data_name, as.matrix(data), start=c(1,1), count=c(nt, n))
	} else {
		for ( st in 1:n ) {
			ncvar_put(nc, data_name, as.matrix(data[,st]), start=c(1,st), count=c(nt, 1))
		}
	}
}
