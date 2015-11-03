#'@title Read timeseries NCDF file
#'
#'
#'@param nc_file A string file path to the nc file to be created.
#'
#'@description
#'This reads a timeseries discrete sampling geometry NCDF file
#'
#'@details
#' TBD
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@import ncdf
#'
#'@export
read_timeseries_dsg = function(nc_file){
	nc<-open.ncdf(nc_file)
	nc_list<-list()
	# Check important global atts
	if(!grepl('CF',att.get.ncdf(nc,0,'Conventions')$value)) {
		warning('File does not advertise CF conventions, unexpected behavior may result.')}
	if(!grepl('timeSeries',att.get.ncdf(nc,0,'featureType')$value)) {
		warning('File does not advertise use of the CF timeseries featureType, unexpected behavior may result.')}
	if(!grepl('Station',att.get.ncdf(nc,0,'cdm_data_type')$value)) {
		warning('File does not advertise use of the Station cdm_data_type, unexpected behavior may result.')}

	# Look for variable with the timeseries_id in it.
	timeseries_id<-NULL
	for(var in nc$var) {
		if(att.get.ncdf(nc,var$name,'standard_name')$value=='station_id') { timeseries_id<-var$name }
		if(att.get.ncdf(nc,var$name,'cf_role')$value=='timeseries_id') { timeseries_id<-var$name }
	}

	# Look for 'coordinates' that match variable names. http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/ch09s05.html
	coord_vars<-list()
	data_vars<-list()
	data_var<-FALSE
	for(var in nc$var) {
		for(coord_var in append(nc$var,nc$dim)) {
			if(grepl(coord_var$name,att.get.ncdf(nc,var$name,'coordinates')$value)) {
				coord_vars<-append(coord_vars,coord_var$name)
				data_var<-TRUE
			}
		}
		if(data_var) { data_vars<-append(data_vars,var$name) }
	}
	
	# Given the coordinates found look for one and only one variable with standard name time, latitude, and longitude. OR (worst case maybe don't support) units like 'days since 1970-01-01 00:00:00', or 'degrees_east', or 'degrees_north'
	lat<-NULL
	lon<-NULL
	alt<-NULL
	time<-NULL
	for(coord_var in coord_vars) {
		for(var in nc$var) {
			if(att.get.ncdf(nc,var$name,'standard_name')$value=='latitude') {lat<-var}
			if(att.get.ncdf(nc,var$name,'standard_name')$value=='longitude') {lon<-var}
			if(att.get.ncdf(nc,var$name,'standard_name')$value=='height') {alt<-var}
			if(att.get.ncdf(nc,var$name,'standard_name')$value=='time') {time<-var}
		for(dim in nc$dim) { # ncdf doesn't treat time as a variable, only a dimension / coordinate variable?
				if(dim$name=="time") {time<-dim}
				if(grepl(' since ',dim$units)) {time<-dim}
			}
		}
	}

	# Check that time unites are: 
	if(!grepl('days since 1970-01-01',time$units)) {stop('Time units other than "days since 1970-01-01" not yet supported.')}

	# Need to check units of lat/lon?
	
	# Return time variable as posixCT -- See Climates package for better netcdf time handling to extend this.
	nc_list$time<-as.POSIXct(time$vals*86400, origin='1970-01-01 00:00.00 UTC')

	# Return lat/lon/alt as they are found.
	nc_list$lats<-get.var.ncdf(nc,lat,1,-1)
	nc_list$lons<-get.var.ncdf(nc,lon,1,-1)
	if(!is.null(alt)){
		nc_list$alts<-get.var.ncdf(nc,alt,1,-1)
	}

# For all variables that have a 'coordinates' attribute that matches the one found earlier... (only implement one for now.)
#### Continue Here.
# 	nc_list$data_unit[1], data$units[1]
# 	nc_list$data_prec[1],'double'
# 	nc_list$varmeta[1]$name,data$variable[1]
# 	nc_list$varmeta[1]$long_name,long_name
# 	nc_list$global_attributes$nc_summary,'test summary'
# 	nc_list$global_attributes$nc_date_create,'2099-01-01'
# 	nc_list$global_attributes$nc_creator_name,'test creator'
# 	nc_list$global_attributes$nc_creator_email,'test@test.com'
# 	nc_list$global_attributes$nc_project,'testthat netcdf.dsg'
# 	nc_list$global_attributes$nc_proc_level,'just a test no processing'
# 	nc_list$global_attributes$nc_title,'test title'
# 	nc_list$data_frames[1],data_frame # Plan to have the dataframes work for 1 to many variables.
	return(nc_list)
}