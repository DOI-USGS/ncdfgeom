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
	# Check these global atts
# 	att.put.ncdf(nc_file, 0,'Conventions','CF-1.7')
# 	att.put.ncdf(nc_file, 0,'featureType','timeSeries')
# 	att.put.ncdf(nc_file, 0,'cdm_data_type','Station')

	# Look for variable with one of:
# att.put.ncdf(nc_file, 'station_name', 'cf_role', 'timeseries_id')
# att.put.ncdf(nc_file, 'station_name','standard_name','station_id')

	# Look for 'coordinates' that match variable names. http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/ch09s05.html
# if(!is.na(alts[1])){
# 	att.put.ncdf(nc_file, data_name, 'coordinates', 'time lat lon alt')
# } else {
# 	att.put.ncdf(nc_file, data_name, 'coordinates', 'time lat lon')
# }

	# Given the coordinates found look for one and only one variable with standard name time, latitude, and longitude. OR (worst case maybe don't support) units like 'days since 1970-01-01 00:00:00', or 'degrees_east', or 'degrees_north'
# att.put.ncdf(nc_file, 'lat', 'standard_name', 'latitude')
# att.put.ncdf(nc_file, 'time', 'standard_name', 'time')
# att.put.ncdf(nc_file, 'lon', 'standard_name', 'longitude')

	# Check that time unites are: 
# time_var 		= var.def.ncdf('time','days since 1970-01-01 00:00:00', time_dim, -999, prec='double', longname='time of measurement')

	# Check that lat/lon units have expected standard names.
# 'degrees_east', or 'degrees_north', or 'height'

	# Return time variable as posixCT
# put.var.ncdf(nc_file, time_var, as.numeric(times)/86400, count=nt) #convert to days since 1970-01-01
# times = as.POSIXct(times)
nc_list$time, time

	# Return lat/lon/alt as they are found.
# put.var.ncdf(nc_file, lat_var, lats, count=n)
# put.var.ncdf(nc_file, lon_var, lons, count=n)
# put.var.ncdf(nc_file, alt_var, alts, count=n)
nc_list$lats, lats
nc_list$lons, lons
nc_list$alts, alts

# For all variables that have a 'coordinates' attribute that matches the one found earlier... (only implement one for now.)
#### Continue Here.
	nc_list$data_unit[1], data$units[1]
	nc_list$data_prec[1],'double'
	nc_list$varmeta[1]$name,data$variable[1]
	nc_list$varmeta[1]$long_name,long_name
	nc_list$global_attributes$nc_summary,'test summary'
	nc_list$global_attributes$nc_date_create,'2099-01-01'
	nc_list$global_attributes$nc_creator_name,'test creator'
	nc_list$global_attributes$nc_creator_email,'test@test.com'
	nc_list$global_attributes$nc_project,'testthat netcdf.dsg'
	nc_list$global_attributes$nc_proc_level,'just a test no processing'
	nc_list$global_attributes$nc_title,'test title'
	nc_list$data_frames[1],data_frame # Plan to have the dataframes work for 1 to many variables.
	return(nc_list)
}