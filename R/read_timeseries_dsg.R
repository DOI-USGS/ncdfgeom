#'@title Read timeseries NCDF file
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'
#'@return nc_list A list containing the contents of the NetCDF file.
#'
#'@description
#'This function reads a timeseries discrete sampling geometry NCDF file and 
#'returns a list of the file's contents.
#'
#'@details
#' The current implementation checks several NetCDF-CF specific conventions prior to attempting to 
#' read the file. The Conventions, featureType, and cdm_data_type global attributes are checked but not
#' strictly required. 
#' 
#' Variables with standard_name and/or cf_rol of station_id and/or timeseries_id are 
#' searched for to indicate which variable is the 'station identifier'. The function stops 
#' if one is not found.
#' 
#' All variables are introspected for a coordinates attribute. This attribute is used to determine
#' which variables are coordinate variables.
#' 
#' The coordinates variables are then introspected and their standard_names used to determine
#' which coordinate they are. Lat, lon, and time are required, height is not. 
#' 
#' Variables with a coordinates attribute are assumed to be the 'data variables'.
#' 
#' Data vars are traversed and their metadata and data content put into lists within the main
#' response list.
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@importFrom ncdf4 nc_open ncatt_get ncvar_get
#'
#'@export
read_timeseries_dsg = function(nc_file){
	nc<-nc_open(nc_file)
	nc_list<-list()
	# Check important global atts
	if(!grepl('CF',ncatt_get(nc,0,'Conventions')$value)) {
		warning('File does not advertise CF conventions, unexpected behavior may result.')}
	if(!grepl('timeSeries',ncatt_get(nc,0,'featureType')$value)) {
		warning('File does not advertise use of the CF timeseries featureType, unexpected behavior may result.')}
	if(!grepl('Station',ncatt_get(nc,0,'cdm_data_type')$value)) {
		warning('File does not advertise use of the Station cdm_data_type, unexpected behavior may result.')}

	# Look for variable with the timeseries_id in it.
	timeseries_id<-NULL
	for(var in nc$var) {
		if(ncatt_get(nc,var$name,'standard_name')$value=='station_id') { timeseries_id<-var$name }
		if(ncatt_get(nc,var$name,'cf_role')$value=='timeseries_id') { timeseries_id<-var$name }
	}
	if(is.null(timeseries_id)) { stop('A timeseries id variable was not found in the file.') }

	# Look for 'coordinates' that match variable names. http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/ch09s05.html
	coord_vars<-list()
	data_vars<-list()
	data_var<-FALSE
	for(var in nc$var) {
		for(coord_var in append(nc$var,nc$dim)) {
			if(grepl(coord_var$name,ncatt_get(nc,var$name,'coordinates')$value)) {
				coord_vars<-append(coord_vars,coord_var$name)
				data_var<-TRUE
			}
		}
		if(data_var) { data_vars<-append(data_vars,var$name) }
	}
	if(length(coord_vars)==0) { stop('No coordinates declarations were found in the file.') }
	
	# Given the coordinates found look for one and only one variable with standard name time, latitude, and longitude. OR (worst case maybe don't support) units like 'days since 1970-01-01 00:00:00', or 'degrees_east', or 'degrees_north'
	lat<-NULL
	lon<-NULL
	alt<-NULL
	time<-NULL
	for(coord_var in coord_vars) {
		for(var in nc$var) {
			if(ncatt_get(nc,var$name,'standard_name')$value=='latitude') {lat<-var}
			if(ncatt_get(nc,var$name,'standard_name')$value=='longitude') {lon<-var}
			if(ncatt_get(nc,var$name,'standard_name')$value=='height') {alt<-var}
			if(ncatt_get(nc,var$name,'standard_name')$value=='time') {time<-var}
		for(dim in nc$dim) { # ncdf doesn't treat time as a variable, only a dimension / coordinate variable?
				if(dim$name=="time") {time<-dim}
				if(grepl(' since ',dim$units)) {time<-dim}
			}
		}
	}
	if(is.null(lat)) { stop('No latitude coordinate found.')}
	if(is.null(lon)) { stop('No longitude coordinate found.')}
	if(is.null(time)) { stop('No time coordinate found.')}
	
	# Check that time unites are: 
	if(!grepl('days since 1970-01-01',time$units)) {stop('Time units other than "days since 1970-01-01" not yet supported.')}
	
	# Return time variable as posixCT -- See Climates package for better netcdf time handling to extend this.
	nc_list$time<-as.POSIXct(time$vals*86400, origin='1970-01-01 00:00.00 UTC')

	# Return lat/lon/alt as they are found.
	nc_list$lats<-ncvar_get(nc,lat,1,-1)
	nc_list$lons<-ncvar_get(nc,lon,1,-1)
	if(!is.null(alt)){
		nc_list$alts<-ncvar_get(nc,alt,1,-1)
	}

	# For all variables that have a 'coordinates' attribute that matches the one found earlier...
	nc_list$varmeta<-list()
	for(data_var in data_vars) {
		nc_list$data_unit[data_var]<-nc$var[data_var][[1]]$units
		nc_list$data_prec[data_var]<-nc$var[data_var][[1]]$prec
		nc_list$varmeta[data_var][[1]]$name<-nc$var[data_var][[1]]$name
		nc_list$varmeta[data_var][[1]]$long_name<-nc$var[data_var][[1]]$longname
		nc_list$data_frames[data_var][[1]]<-as.data.frame(ncvar_get(nc,data_var,c(1,1),c(-1,-1)))
		colnames(nc_list$data_frames[data_var][[1]])<-as.character(ncvar_get(nc,timeseries_id))
	}
	
	nc_list$global_attributes$nc_summary<-ncatt_get(nc,0,'summary')$value
	nc_list$global_attributes$nc_date_created<-ncatt_get(nc,0,'date_created')$value
	nc_list$global_attributes$nc_creator_name<-ncatt_get(nc,0,'creator_name')$value
	nc_list$global_attributes$nc_creator_email<-ncatt_get(nc,0,'creator_email')$value
	nc_list$global_attributes$nc_project<-ncatt_get(nc,0,'project')$value
	nc_list$global_attributes$nc_proc_level<-ncatt_get(nc,0,'processing_level')$value
	nc_list$global_attributes$nc_title<-ncatt_get(nc,0,'title')$value
	return(nc_list)
}