#'@title Read NetCDF-CF timeSeries featuretype
#'
#'@param nc_file character file path to the nc file to be read.
#'
#'@return list containing the contents of the NetCDF file.
#'
#'@description
#'This function reads a timeseries discrete sampling geometry NetCDF file and 
#'returns a list containing the file's contents.
#'
#'@details
#' The current implementation checks several NetCDF-CF specific conventions prior to attempting to 
#' read the file. The Conventions and featureType global attributes are checked but not
#' strictly required. 
#' 
#' Variables with standard_name and/or cf_role of station_id and/or timeseries_id are 
#' searched for to indicate which variable is the 'timeseries identifier'. The function stops 
#' if one is not found.
#' 
#' All variables are introspected for a coordinates attribute. This attribute is used to determine
#' which variables are coordinate variables. If none are found an attempt to infer data 
#' variables by time and timeseries_id dimensions is made.
#' 
#' The coordinates variables are introspected and their standard_names used to determine
#' which coordinate they are. Lat, lon, and time are required, height is not. 
#' 
#' Variables with a coordinates attribute are assumed to be the 'data variables'.
#' 
#' Data variables are traversed and their metadata and data content put into lists within the main
#' response list.
#' 
#' See the timeseries vignette for more information.
#'
#'@references
#'http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html
#'
#'@importFrom RNetCDF open.nc var.get.nc var.inq.nc utcal.nc close.nc
#'@importFrom ncmeta nc_meta
#'@importFrom dplyr filter select group_by
#'
#'@export
read_timeseries_dsg = function(nc_file){
  
  # unquoted variable hack
  variable <- attribute <- value <- dimension <- name <- NULL
  
	nc <- open.nc(nc_file)
	on.exit(close.nc(nc), add  = TRUE)
	
	nc_meta <- nc_meta(nc_file)
	nc_atts <- nc_meta$attribute
	
	nc_list<-list()
	
	# Check important global atts
	check <- filter(nc_atts, variable == "NC_GLOBAL" & attribute == 'Conventions')$value
	if(length(check) == 0 || !grepl('CF', check)) {
		warning('File does not advertise CF conventions, unexpected behavior may result.') 
	}
		
	check <- filter(nc_atts, variable == "NC_GLOBAL" & attribute == "featureType")$value
	if(length(check) == 0 || !grepl('timeSeries', check)) {
		warning('File does not advertise use of the CF timeseries featureType, unexpected behavior may result.') 
	 }

	# Look for variable with the timeseries_id in it.
	timeseries_id <- filter(nc_atts, attribute == "standard_name" &
	                          value == "station_id")$variable
	timeseries_id <- filter(nc_atts, attribute == "cf_role" &
	                          value == pkg.env$timeseries_id_cf_role)$variable
	
	if(length(timeseries_id) == 0) { 
	  stop('A timeseries id variable was not found in the file.') 
	  }

	# Look for 'coordinates' that match variable names. 
	# http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/ch09s05.html
	nc_coord_vars <- ncmeta::nc_coord_var(nc)
	
	if (nrow(nc_coord_vars) == 0) { 
	  stop('No coordinates declarations were found in the file.') 
	}
	
	coord_vars <- unique(c(nc_coord_vars$X, nc_coord_vars$Y, 
	                       nc_coord_vars$Z, nc_coord_vars$T))
	coord_vars <- coord_vars[!is.na(coord_vars)]
	
	data_vars <- filter(nc_atts, attribute == "coordinates" & 
	                      grepl(paste(coord_vars, collapse = "|"), value))
	
	# Given the coordinates found look for one and only one variable 
	# with standard name time, latitude, and longitude. 
	# OR (worst case maybe don't support) units like 'days since 1970-01-01 00:00:00', 
	# or 'degrees_east', or 'degrees_north'
	
	sn <- filter(nc_atts, attribute == "standard_name")
	lat <- filter(sn, value == pkg.env$lat_coord_var_standard_name)
	lon <- filter(sn, value == pkg.env$lon_coord_var_standard_name)
	alt <- filter(sn, value == pkg.env$alt_coord_var_standard_name)
	time <- filter(sn, value == pkg.env$time_var_standard_name)
	
	if(nrow(time) == 0) {
	  time <- filter(nc_atts, attribute == "units" & grepl(" since ", value))
	}
	
	if(length(lat) == 0) { stop('No latitude coordinate found.') }
	if(length(lon) == 0) { stop('No longitude coordinate found.') }
	if(length(time) == 0) { stop('No time coordinate found.') }
	
	axes <- nc_meta$axis
	dim_time <- filter(axes, variable == time$variable)$dimension
	dim_tsid <- filter(axes, variable == timeseries_id)$dimension
	
	if (nrow(data_vars) == 0) {
	  warning("no data variables found, attempting to infer via shared dimensions")
    
	  axes_search <- group_by(axes, variable)
	  axes_search <- filter(axes_search, sum(c(dim_tsid, dim_time) %in% dimension) == 2 &
	                   !variable %in% c(coord_vars, timeseries_id))
	  
	  data_vars <- data.frame(variable = unique(axes_search$variable), stringsAsFactors = FALSE)
	  
	  if(nrow(data_vars) == 0) stop("No data variables could be identified")
	}
	
	time_vals <- var.get.nc(nc, time$variable)
	time_units <- filter(nc_atts, variable == time$variable & attribute == "units")
	
	nc_list$time <- utcal.nc(time_units$value[[1]], time_vals, type = "c")

	if(nrow(lat) > 0) {
	  nc_list$lats <- var.get.nc(nc, lat$variable)
	} else {
	  warning("no latitude coordinate found")
	  nc_list$lats <- numeric(0)
	}
	
	if(nrow(lon) > 0) {
	  nc_list$lons <- var.get.nc(nc, lon$variable)
	} else {
	  warning("no longitude coordinate found")
	  nc_list$lons <- numeric(0)
	}
	
	if(nrow(alt) > 0) { 
	  nc_list$alts <- var.get.nc(nc, alt$variable)
	} else {
	  warning("no altitude coordinate found")
	  nc_list$alts <- numeric(0)
	}

	# For all variables that have a 'coordinates' attribute that matches the one found earlier...
	nc_list$varmeta <- list()
	
	for(data_var in data_vars$variable) {
	  nc_var <- filter(nc_meta$variable, name == data_var)
	  
	  nc_list$data_unit[data_var] <- filter(nc_atts, variable == data_var &
		                                        attribute == "units")$value[[1]]
		nc_list$data_prec[data_var] <- nc_var$type # todo map this to NetCDF types
		
		nc_list$varmeta[data_var][[1]]$name <- data_var
		nc_list$varmeta[data_var][[1]]$long_name <- filter(nc_atts, variable == data_var &
		                                                     attribute == "long_name")$value[[1]]
		# Ensures we get back data with time in rows.
		dim_order <- match(var.inq.nc(nc, data_var)$dimids, 
		                   c(dim_time, dim_tsid))
		
		nc_list$data_frames[data_var][[1]] <- as.data.frame(var.get.nc(nc, data_var))
		
		if(dim_order[1] > dim_order[2])
		  nc_list$data_frames[data_var][[1]] <- t(nc_list$data_frames[data_var][[1]])
		
		colnames(nc_list$data_frames[data_var][[1]]) <- as.character(var.get.nc(nc, timeseries_id))
	}
	
	nc_list$global_attributes$nc_summary <- filter(nc_atts, variable == "NC_GLOBAL" &
	                                                 attribute == "summary")$value
	nc_list$global_attributes$nc_date_created <- filter(nc_atts, variable == "NC_GLOBAL" &
	                                                      attribute == "date_created")$value
	nc_list$global_attributes$nc_creator_name <- filter(nc_atts, variable == "NC_GLOBAL" &
	                                                      attribute == "creator_name")$value
	nc_list$global_attributes$nc_creator_email <- filter(nc_atts, variable == "NC_GLOBAL" &
	                                                       attribute == "creator_email")$value
	nc_list$global_attributes$nc_project <- filter(nc_atts, variable == "NC_GLOBAL" &
	                                                 attribute == "project")$value
	nc_list$global_attributes$nc_proc_level <- filter(nc_atts, variable == "NC_GLOBAL" &
	                                                    attribute == "processing_level")$value
	nc_list$global_attributes$nc_title <- filter(nc_atts, variable == "NC_GLOBAL" &
	                                               attribute == "title")$value
	return(nc_list)
}