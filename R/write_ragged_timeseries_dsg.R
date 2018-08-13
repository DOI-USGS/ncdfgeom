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
#'@param data_units Optional list of observation units.  Names must correspond
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
#'@import ncdf4
#'@importFrom dplyr left_join
#'
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
	
	all_data = all_data[order(all_data$station_name), ]
	
	sta_obs = rle(all_data$station_name)
	sta_obs = data.frame(station_name=sta_obs$values, row_size=sta_obs$lengths, stringsAsFactors = FALSE)
	
	sta_metadata = all_data[, c('station_name', 'lat', 'lon', 'alt')]
	sta_obs = dplyr::left_join(sta_obs, dplyr::distinct(sta_metadata), by = "station_name")
	
	num_rows = nrow(all_data)
	num_stations = nrow(sta_obs)
	
	#Lay the foundation. This is a ragged array. Which has two dimensions, "obs" and "station"
	obs_dim = ncdim_def('obs', '', 1:num_rows, create_dimvar=FALSE)
	station_dim = ncdim_def('station', '', 1:num_stations, create_dimvar=FALSE)
	
	strlen_dim = ncdim_def('name_strlen', '', 1:max(sapply(all_data$station_name, nchar)), create_dimvar=FALSE)
	
	#Setup our spatial and time info
	station_var = ncvar_def('station_name', '', list(strlen_dim, station_dim), prec='char', longname='Station Names')
	row_var     = ncvar_def('row_size', '', station_dim, missval=0, prec='integer', longname='number of observations for this station')
	time_var 		= ncvar_def('time','days since 1970-01-01 00:00:00', obs_dim, -999, prec='double', longname='time of measurement')
	lat_var 		= ncvar_def('lat', 'degrees_north', station_dim, -999, prec='double', longname = 'latitude of the observation')
	lon_var 		= ncvar_def('lon', 'degrees_east', station_dim, -999, prec='double', longname = 'longitude of the observation')
	alt_var 		= ncvar_def('alt', 'm', station_dim, -999, prec='double', longname='vertical distance above the surface')
	
	#now do the observation data itself
	timeseries_data = all_data[, !(names(all_data) %in% required_cols)]
	data_names = names(timeseries_data)
	data_vars = list()
	
	for(i in 1:length(data_names)){
		unit     = ifelse(is.null(data_units[[data_names[i]]]), '', data_units[[data_names[i]]])
		
		data_vars[[i]] = ncvar_def(data_names[i], unit, obs_dim, prec=data_prec, missval=-999)
	}
	
	nc = nc_create(nc_file, vars = c(list(lat_var, lon_var, alt_var, time_var, station_var, row_var), data_vars))
	
	#add standard_names
	ncatt_put(nc, 'lat', 'standard_name', 'latitude')
	ncatt_put(nc, 'time', 'standard_name', 'time')
	ncatt_put(nc, 'lon', 'standard_name', 'longitude')
	ncatt_put(nc, 'alt', 'standard_name', 'height')
	ncatt_put(nc, 'station_name', 'cf_role', 'timeseries_id')
	
	#some final stuff
	ncatt_put(nc, 0,'Conventions','CF-1.7')
	ncatt_put(nc, 0,'featureType','timeSeries')
	ncatt_put(nc, 0,'cdm_data_type','Station')
	ncatt_put(nc, 0,'standard_name_vocabulary','CF-1.7')
	
	nc_close(nc)
	nc <- nc_open(nc_file, write = TRUE)
	
	#Put data in NC file
	ncvar_put(nc, 'time', as.numeric(all_data$time)/86400, count=num_rows) #convert to days since 1970-01-01
	ncvar_put(nc, 'lat', sta_obs$lat, count=num_stations)
	ncvar_put(nc, 'lon', sta_obs$lon, count=num_stations)
	ncvar_put(nc, 'alt', sta_obs$alt, count=num_stations)
	ncvar_put(nc, 'row_size', sta_obs$row_size, count=num_stations)
	
	ncvar_put(nc, 'station_name', sta_obs$station_name, count=c(-1,num_stations))
	
	data_names = names(timeseries_data)
	data_vars = list()
	
	for(i in 1:length(data_names)){	
		data_name = ifelse(is.null(data_longnames[[data_names[i]]]), '', data_longnames[[data_names[i]]])
		
		ncatt_put(nc, data_names[i], 'standard_name', data_name)
		ncatt_put(nc, data_names[i], 'coordinates', 'time lat lon')
		
		ncvar_put(nc, data_names[i], timeseries_data[, data_names[i]], start=1, count=num_rows)
	}
	
	#Add the optional global attributes
	if(length(attributes) > 0){
		for(i in 1:length(attributes)){
			ncatt_put(nc, 0, names(attributes[i]), attributes[[i]])
		}
	}
	
	nc_close(nc)
}
