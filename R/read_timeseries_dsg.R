#'@title Read NetCDF-CF timeSeries featuretype
#'
#'@param nc_file character file path to the nc file to be read.
#'@param read_data logical whether to read metadata only or not.
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
#'https://www.unidata.ucar.edu/software/netcdf-java/v4.6/reference/FeatureDatasets/CFpointImplement.html
#'
#'@importFrom RNetCDF open.nc var.get.nc var.inq.nc utcal.nc close.nc
#'@importFrom ncmeta nc_meta
#'@importFrom dplyr filter select group_by
#'
#'@export
read_timeseries_dsg = function(nc_file, read_data = TRUE){
  
	nc <- open.nc(nc_file)
	on.exit(close.nc(nc), add  = TRUE)
	
	nc_meta <- get_nc_meta(nc)
	
	dsg <- get_dsg_meta(nc, nc_meta)
	
  nc_list <- get_nc_list(nc, dsg, nc_meta, read_data)
	
	return(add_globals(nc_list, nc_meta))
}

get_nc_meta <- function(nc) {
  nc_meta <- nc_meta(nc)
  nc_meta$attribute <- check_timeseries_atts(nc_meta$attribute)
  
  nc_meta
}

check_timeseries_atts <- function(nc_atts) {
  
  if(!"name" %in% names(nc_atts)) {
    names(nc_atts) <- c("name", "variable", "value")
  }
  
  # Check important global atts
  check <- filter(nc_atts, .data$variable == "NC_GLOBAL" & .data$name == 'Conventions')$value
  if(length(check) == 0 || !grepl('CF', check)) {
    warning('File does not advertise CF conventions, unexpected behavior may result.') 
  }
  
  check <- filter(nc_atts, .data$variable == "NC_GLOBAL" & .data$name == "featureType")$value
  if(length(check) == 0 || !grepl('timeSeries', check)) {
    warning('File does not advertise use of the CF timeseries featureType, unexpected behavior may result.') 
  }
  
  nc_atts  
}

get_dsg_meta <- function(nc, nc_meta = NULL) {
  
  if(is.null(nc_meta)) nc_meta <- get_nc_meta(nc)

  dsg <- list()
  
  dsg$timeseries_id <- get_timeseries_id(nc_meta$attribute)
  
  dsg$coord_vars <- get_coord_vars(nc)
  
  dsg$data_vars <- filter(nc_meta$attribute, .data$name == "coordinates" & 
                            grepl(paste(dsg$coord_vars, collapse = "|"), .data$value))
  
  # Given the coordinates found look for one and only one variable 
  # with standard name time, latitude, and longitude. 
  # OR (worst case maybe don't support) units like 'days since 1970-01-01 00:00:00', 
  # or 'degrees_east', or 'degrees_north'
  
  dsg$sn <- filter(nc_meta$attribute, .data$name == "standard_name")
  dsg$lat <- filter(dsg$sn, .data$value == pkg.env$lat_coord_var_standard_name)
  dsg$lon <- filter(dsg$sn, .data$value == pkg.env$lon_coord_var_standard_name)
  dsg$alt <- filter(dsg$sn, .data$value == pkg.env$alt_coord_var_standard_name)
  dsg$time <- filter(dsg$sn, .data$value == pkg.env$time_var_standard_name)
  
  if(nrow(dsg$time) == 0) {
    dsg$time <- filter(nc_meta$attribute, .data$name == "units" & grepl(" since ", .data$value))
  }
  
  if(length(dsg$lat) == 0) { stop('No latitude coordinate found.') }
  if(length(dsg$lon) == 0) { stop('No longitude coordinate found.') }
  if(length(dsg$time) == 0) { stop('No time coordinate found.') }
  
  dsg$dim_time <- filter(nc_meta$axis, .data$variable == dsg$time$variable)$dimension
  dsg$dim_tsid <- filter(nc_meta$axis, .data$variable == dsg$timeseries_id)$dimension
  
  if (nrow(dsg$data_vars) == 0) {
    warning("no data variables found, attempting to infer via shared dimensions")
    
    axes_search <- group_by(nc_meta$axis, .data$variable)
    axes_search <- filter(axes_search, sum(c(dsg$dim_tsid, dsg$dim_time) %in% .data$dimension) == 2 &
                            !.data$variable %in% c(dsg$coord_vars, dsg$timeseries_id))
    
    dsg$data_vars <- data.frame(variable = unique(axes_search$variable), stringsAsFactors = FALSE)
    
    if(nrow(dsg$data_vars) == 0) stop("No data variables could be identified")
  }
  
  dsg$var_meta <- list()
  
  for(data_var in dsg$data_vars$variable) {
    dsg$varmeta[[data_var]] <- list() 
    
    dsg$varmeta[[data_var]]$nc_var <- filter(nc_meta$variable, .data$name == data_var)
    
    # Ensures we get back data with time in rows.
    var_inq <- var.inq.nc(nc, data_var)
    if(var_inq$type == "NC_CHAR") {
      dsg$varmeta[[data_var]]$dims <- var_inq$dimids[var_inq$dimids %in% c(dsg$dim_time, dsg$dim_tsid)]
    } else {
      dsg$varmeta[[data_var]]$dims <- var_inq$dimids
    }
    
    dsg$varmeta[[data_var]]$dim_order <- match(dsg$varmeta[[data_var]]$dims, 
                                               c(dsg$dim_time, dsg$dim_tsid))
    
  }
  
  dsg
}

get_nc_list <- function(nc, dsg, nc_meta, read_data) {
  nc_list<-list()
  
  time_vals <- var.get.nc(nc, dsg$time$variable)
  time_units <- filter(nc_meta$attribute, .data$variable == dsg$time$variable & .data$name == "units")
  
  nc_list$time <- utcal.nc(time_units$value[[1]], time_vals, type = "c")
  
  if(nrow(dsg$lat) > 0) {
    nc_list$lats <- var.get.nc(nc, dsg$lat$variable)
  } else {
    warning("no latitude coordinate found")
    nc_list$lats <- numeric(0)
  }
  
  if(nrow(dsg$lon) > 0) {
    nc_list$lons <- var.get.nc(nc, dsg$lon$variable)
  } else {
    warning("no longitude coordinate found")
    nc_list$lons <- numeric(0)
  }
  
  if(nrow(dsg$alt) > 0) { 
    nc_list$alts <- var.get.nc(nc, dsg$alt$variable)
  } else {
    warning("no altitude coordinate found")
    nc_list$alts <- numeric(0)
  }
  
  # For all variables that have a 'coordinates' attribute that matches the one found earlier...
  nc_list$varmeta <- list()
  
  for(data_var in dsg$data_vars$variable) {
    
    nc_list$data_unit[data_var] <- filter(nc_meta$attribute, .data$variable == data_var &
                                            .data$name == "units")$value[[1]]
    nc_list$data_prec[data_var] <- dsg$varmeta[[data_var]]$nc_var$type # todo map this to NetCDF types
    
    nc_list$varmeta[data_var][[1]]$name <- data_var
    nc_list$varmeta[data_var][[1]]$long_name <- filter(nc_meta$attribute, .data$variable == data_var &
                                                         .data$name == "long_name")$value[[1]]
    
  }
  
  if(read_data) {
    for(data_var in dsg$data_vars$variable) {
      nc_list$data_frames[data_var][[1]] <- as.data.frame(var.get.nc(nc, data_var))
      
      if(dsg$varmeta[[data_var]]$dim_order[1] > dsg$varmeta[[data_var]]$dim_order[2])
        nc_list$data_frames[data_var][[1]] <- t(nc_list$data_frames[data_var][[1]])
      
      colnames(nc_list$data_frames[data_var][[1]]) <- as.character(var.get.nc(nc, dsg$timeseries_id))
    }
  }
  
  nc_list
}

get_timeseries_id <- function(nc_atts) {
  # Look for variable with the timeseries_id in it.
  timeseries_id <- filter(nc_atts, .data$name == "standard_name" &
                            .data$value == "station_id")$variable
  timeseries_id <- filter(nc_atts, .data$name == "cf_role" &
                            .data$value == pkg.env$timeseries_id_cf_role)$variable
  
  if(length(timeseries_id) == 0) { 
    stop('A timeseries id variable was not found in the file.') 
  }
  
  timeseries_id
}

get_coord_vars <- function(nc) {
  # Look for 'coordinates' that match variable names. 
  # http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/ch09s05.html
  nc_coord_vars <- ncmeta::nc_coord_var(nc)
  
  if (nrow(nc_coord_vars) == 0) { 
    stop('No coordinates declarations were found in the file.') 
  }
  
  coord_vars <- unique(c(nc_coord_vars$X, nc_coord_vars$Y, 
                         nc_coord_vars$Z, nc_coord_vars$T))
  coord_vars[!is.na(coord_vars)]
}

.data <- NULL

add_globals <- function(nc_list, nc_meta) {
  nc_list$global_attributes$nc_summary <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                   .data$name == "summary")$value
  nc_list$global_attributes$nc_date_created <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                        .data$name == "date_created")$value
  nc_list$global_attributes$nc_creator_name <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                        .data$name == "creator_name")$value
  nc_list$global_attributes$nc_creator_email <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                         .data$name == "creator_email")$value
  nc_list$global_attributes$nc_project <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                   .data$name == "project")$value
  nc_list$global_attributes$nc_proc_level <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                      .data$name == "processing_level")$value
  nc_list$global_attributes$nc_title <- filter(nc_meta$attribute, .data$variable == "NC_GLOBAL" &
                                                 .data$name == "title")$value
  
  attr(nc_list, "class") <- "ncdfgeom"
  
  nc_list
}