#'@title Create ragged array timeseries NCDF file
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'
#'@param all_data A data.frame object containing all station and observation data. See 
#'description for further information
#'
#'@param data_longnames Optional named list of observation long names. Names must correspond
#'to names of observation columns in \code{all_data}. Not required. Columns without 
#'supplied long name are given a long name of empty string.
#'
#'@param data_unit Optional list of observation units.  Names must correspond
#'to names of observation columns in \code{all_data}. Not required. Columns without 
#'supplied data unit are given a unit of empty string.
#'
#'@param attributes An optional list of attributes that will be added at the global level. 
#'See details for useful attributes.
#'
#'@description
#'This creates a ragged array timeseries discrete sampling features NCDF file
#'
#'@details
#'It's challenging to create a function that accepts the style of data that could go into
#'the ragged array timeseries DSG. This is one attempt, though ideas and feedback are welcome.
#'
#'The all_data data.frame is the heart of the data passed. It \emph{must} have the following
#'columns. 
#'\itemize{
#'	\item{station_name}
#'	\item{lat}
#'	\item{lon}
#'	\item{alt}
#'	\item{time}
#'	\item{observation1}
#'	\item{...}
#'	\item{observationN}
#'}
#'
#'observation1 and observationN are just placeholders. Observations should be described with 
#'a descriptive name. There can be any number of observations, but their names must be unique. 
#'
#'Attributes allow for addition, non-core metadata to be included. Must be a named list. All
#'items in the list will be added as global attributes to the NC file with the supplied name/value 
#'pair. 
#'
#'Here are some common important attributes that could be added.
#'\itemize{
#'	\item{title = "title"}
#'	\item{abstract = "history"}
#'	\item{provider site = "institution"}
#'	\item{provider name ="source"}
#'	\item{description = "description"}
#'}
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@import ncdf4
#'
#'@examples
#'\dontrun{
#'library(soilmoisturetools)
#'ok = ok_data()
#'ok_meta = ok_sites_metadata(ok$station)[, c('station', 'latitude', 'longitude', 'elevation')]
#'all_data = merge(ok, ok_meta)
#'
#'names(all_data) = c('station_name', 'time', 'soil_moisture', 'soil_moisture_depth', 'lat', 'lon', 'alt')
#'write_ragged_timeseries_dsg(path.expand('~/test.nc'), all_data, list(soil_moisture='%', soil_moisture_depth='inches'))
#'
#'}
#'@export
write_ragged_timeseries_dsg = function(nc_file, all_data, data_units=list(), data_longnames=list(), attributes=list()){
	
	#building this with the convention shown here:
	# http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/cf-conventions.html#idp9804080
	
	required_cols = c('station_name', 'lat', 'lon', 'alt', 'time')
	data_prec = 'double'
	
	if(!all(required_cols %in% names(all_data))){
		stop('all_data must include the following columns: ', paste(required_cols, collapse = ' '))
	}
	
	if(!is(all_data$time, 'POSIXct')){
		all_data$time = as.POSIXct(all_data$time)
	}
	
	#Lets prep station names and
	
	all_data = all_data[order(all_data$station_name), ]
	
	sta_obs = rle(all_data$station_name)
	sta_obs = data.frame(station_name=sta_obs$values, row_size=sta_obs$lengths)
	
	sta_metadata = all_data[, c('station_name', 'lat', 'lon', 'alt')]
	sta_obs = merge(sta_obs, unique(sta_metadata), all.x=TRUE, all.y=FALSE)
	
	n = nrow(all_data)
	n_sta = nrow(sta_obs)
	
	#Lay the foundation. This is a ragged array. Which has two dimensions, "obs" and "station"
	obs_dim = dim.def.ncdf('obs', '', 1:n, unlim=FALSE, create_dimvar=FALSE)
	station_dim = dim.def.ncdf('station', '', 1:n_sta, unlim=TRUE, create_dimvar=FALSE)
	
	strlen_dim = dim.def.ncdf('name_strlen', '', 1:max(sapply(all_data$station_name, nchar)), create_dimvar=FALSE)
	
	#Setup our spatial and time info
	station_var = var.def.ncdf('station_name', '', list(strlen_dim, station_dim), missval='', prec='char', longname='Station Names')
	row_var     = var.def.ncdf('row_size', '', station_dim, missval=0, prec='integer', longname='number of observations for this station')
	time_var 		= var.def.ncdf('time','days since 1970-01-01 00:00:00', obs_dim, -999, prec='double', longname='time of measurement')
	lat_var 		= var.def.ncdf('lat', 'degrees_north', station_dim, -999, prec='double', longname = 'latitude of the observation')
	lon_var 		= var.def.ncdf('lon', 'degrees_east', station_dim, -999, prec='double', longname = 'longitude of the observation')
	alt_var 		= var.def.ncdf('alt', 'm', station_dim, -999, prec='double', longname='vertical distance above the surface')
	
	#now do the observation data itself
	data = all_data[, !(names(all_data) %in% required_cols)]
	data_names = names(data)
	data_vars = list()
	
	for(i in 1:length(data_names)){
		unit     = ifelse(is.null(data_units[[data_names[i]]]), '', data_units[[data_names[i]]])
		
		data_vars[[i]] = var.def.ncdf(data_names[i], unit, obs_dim, prec=data_prec, missval=-999)
	}
	
	#nc_file = create.ncdf(nc_file, vars = c(list(lat_var, lon_var, alt_var, time_var, station_var), data_vars))
	nc_file = create.ncdf(nc_file, vars = c(list(lat_var, lon_var, alt_var, time_var, station_var, row_var), data_vars))
	
	#add standard_names
	att.put.ncdf(nc_file, 'lat', 'standard_name', 'latitude')
	att.put.ncdf(nc_file, 'time', 'standard_name', 'time')
	att.put.ncdf(nc_file, 'lon', 'standard_name', 'longitude')
	att.put.ncdf(nc_file, 'alt', 'standard_name', 'height')
	att.put.ncdf(nc_file, 'station_name', 'cf_role', 'timeseries_id')
	
	#some final stuff
	att.put.ncdf(nc_file, 0,'Conventions','CF-1.7')
	att.put.ncdf(nc_file, 0,'featureType','timeSeries')
	att.put.ncdf(nc_file, 0,'cdm_data_type','Station')
	att.put.ncdf(nc_file, 0,'standard_name_vocabulary','CF-1.7')
	
	#Put data in NC file
	put.var.ncdf(nc_file, 'time', as.numeric(all_data$time)/86400, count=n) #convert to days since 1970-01-01
	put.var.ncdf(nc_file, 'lat', sta_obs$lat, count=n_sta)
	put.var.ncdf(nc_file, 'lon', sta_obs$lon, count=n_sta)
	put.var.ncdf(nc_file, 'alt', sta_obs$alt, count=n_sta)
	put.var.ncdf(nc_file, 'row_size', sta_obs$row_size, count=n_sta)
	
	put.var.ncdf(nc_file, 'station_name', sta_obs$station_name, count=c(-1,n_sta))
	
	data_names = names(data)
	data_vars = list()
	
	for(i in 1:length(data_names)){	
		data_name = ifelse(is.null(data_longnames[[data_names[i]]]), '', data_longnames[[data_names[i]]])
		
		att.put.ncdf(nc_file, data_names[i], 'standard_name', data_name)
		att.put.ncdf(nc_file, data_names[i], 'coordinates', 'time lat lon')
		
		put.var.ncdf(nc_file, data_names[i], data[, data_names[i]], start=1, count=n)
	}
	
	#Add the optional global attributes
	if(length(attributes) > 0){
		for(i in 1:length(attributes)){
			att.put.ncdf(nc_file, 0, names(attributes[i]), attributes[[i]])
		}
	}
	
	close.ncdf(nc_file)
}
