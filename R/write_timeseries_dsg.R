#' @title Write orthoganal array time series to NetCDF-CF
#'
#' @param nc_file \code{character} file path to the nc file to be created.
#' @param instance_names \code{character} or \code{numeric} vector of names for each instance 
#' (e.g. station or geometry) to be added to the file.
#' @param times \code{POSIXct} vector of times. Must be of type \code{POSIXct} or an attempt to 
#' convert it will be made using \code{as.POSIXct(times)}.
#' @param lats \code{numeric} vector of latitudes 
#' @param lons \code{numeric} vector of longitudes
#' @param data \code{data.frame} with each column corresponding to an instance. Rows correspond to 
#' time steps. nrow must be the same length as times. Column names must match instance names.
#' @param alts \code{numeric} vector of altitudes (m above sea level) (Optional)
#' @param data_unit \code{character} vector of data units. Length must be the same as number 
#' of columns in \code{data} parameter.
#' @param data_prec \code{character} precision of observation data in NetCDF file. 
#' Valid options: 'short' 'integer' 'float' 'double' 'char'.
#' @param data_metadata \code{list} A named list of strings: list(name='ShortVarName', long_name='A Long Name')
#' @param attributes list An optional list of attributes that will be added at the global level. 
#' See details for useful attributes.
#' @param add_to_existing \code{boolean} If TRUE and the file already exists, 
#' variables will be added to the existing file. See details for more.
#' 
#' @description
#' This function creates a timeseries discrete sampling geometry NetCDF file.
#' It uses the orthogonal array encoding to write one \code{data.frame} per
#' function call. This encoding is best suited to data with the same number of
#' timesteps per instance (e.g. geometry or station).
#' 
#' @details
#' Suggested Global Variables:
#' c(title = "title", 
#' abstract = "history", 
#' provider site = "institution", 
#' provider name ="source", 
#' description = "description")
#' 
#' Note regarding add_to_existing:
#' add_to_existing = TRUE should only be used to add variables to an existing 
#' NetCDF discrete sampling geometry file. All other inputs should be the 
#' same as are already in the file. If the functions is called with 
#' add_to_existing=FALSE (the default), it will overwrite an existing file 
#' with the same name. The expected usage is to call this function repeatedly 
#' only changing the data, data_unit, data_prec and data_metadata inputs.
#' 
#' See the timeseries vignette for more information.
#' 
#' @references
#' \enumerate{
#'   \item \url{http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/FeatureDatasets/CFpointImplement.html}
#'   \item \url{http://cfconventions.org/cf-conventions/cf-conventions.html#_orthogonal_multidimensional_array_representation}
#'   \item \url{http://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/cf-conventions.html#time-series-data}
#' }
#' 
#' @importFrom RNetCDF open.nc close.nc create.nc dim.def.nc var.def.nc var.put.nc att.put.nc
#' @importFrom methods is
#' 
#' @export
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
	
	if(ncol(data) != n){
		stop('number of data columns must equal the number of stations')
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
	  orig_nc <- nc_file
	  nc_file <- tempfile()
	  file.copy(orig_nc, nc_file)
	  
		nc<-open.nc(nc_file, write = TRUE)
		data_vars = list()
		
		add_var(nc, data_name, c(pkg.env$time_dim_name, pkg.env$instance_dim_name), 
		        pkg.env$nc_types[data_prec][[1]], data_unit, 
		        long_name = data_metadata[['long_name']], data = data)
    
		close.nc(nc)
		nc<-open.nc(nc_file, write = TRUE)
		
		put_data_in_nc(nc,nt,n,data_name,data, alts)
		
		close.nc(nc)
		
		if(add_to_existing) file.rename(nc_file, orig_nc)
		
	} else {
	  nc <- create.nc(nc_file)
	  
		dim.def.nc(nc, pkg.env$instance_dim_name, n, unlim = FALSE)
		dim.def.nc(nc, pkg.env$time_dim_name, nt, unlim=FALSE)
		
		#Setup our spatial and time info
		add_var(nc, pkg.env$dsg_timeseries_id, 
		        c(pkg.env$instance_dim_name), 
		        "NC_CHAR", long_name = "Station Names", 
		        data = instance_names)
		
		add_var(nc, pkg.env$time_var_name, pkg.env$time_dim_name, "NC_DOUBLE", 
		        'days since 1970-01-01 00:00:00', -999, 'time of measurement')
		
		add_var(nc, pkg.env$lat_coord_var_name, pkg.env$instance_dim_name, "NC_DOUBLE", 
		        'degrees_north', -999, 'latitude of the observation')
		
		add_var(nc, pkg.env$lon_coord_var_name, pkg.env$instance_dim_name, "NC_DOUBLE", 
		        'degrees_east', -999, 'longitude of the observation')
		
		if(!is.na(alts[1])){
		  add_var(nc, pkg.env$alt_coord_var_name, pkg.env$instance_dim_name, "NC_DOUBLE", 
		          'm', -999, 'vertical distance above the surface')
		}
		
    add_var(nc, data_name, c(pkg.env$time_dim_name, pkg.env$instance_dim_name), 
            pkg.env$nc_types[data_prec][[1]], data_unit, -999, data_metadata[['long_name']])

		close.nc(nc)
		nc <- open.nc(nc_file, write = TRUE)

		#add standard_names
		att.put.nc(nc, pkg.env$lat_coord_var_name, 'standard_name', "NC_CHAR", pkg.env$lat_coord_var_standard_name)
		att.put.nc(nc, pkg.env$time_var_name, 'standard_name', "NC_CHAR", pkg.env$time_var_standard_name)
		att.put.nc(nc, pkg.env$lon_coord_var_name, 'standard_name', "NC_CHAR", pkg.env$lon_coord_var_standard_name)
		
		if(!is.na(alts[1])){
			att.put.nc(nc, pkg.env$alt_coord_var_name, 'standard_name', "NC_CHAR", pkg.env$alt_coord_var_standard_name)
		}
		
		att.put.nc(nc, pkg.env$dsg_timeseries_id, 'cf_role', "NC_CHAR", pkg.env$timeseries_id_cf_role)
		
		#Important Global Variables
		att.put.nc(nc, "NC_GLOBAL", 'Conventions', "NC_CHAR", pkg.env$cf_version)
		att.put.nc(nc, "NC_GLOBAL", 'featureType', "NC_CHAR", 'timeSeries')
		att.put.nc(nc, "NC_GLOBAL", 'cdm_data_type', "NC_CHAR", 'Station')
		att.put.nc(nc, "NC_GLOBAL", 'standard_name_vocabulary', "NC_CHAR", pkg.env$cf_version)
		
		#Add the optional global attributes
		if(length(attributes)>0){
			for(i in 1:length(attributes)){
				att.put.nc(nc, "NC_GLOBAL", names(attributes)[i], 
				           pkg.env$nc_types[class(attributes[[i]])][[1]], attributes[[i]])
			}
		}
		
		#Put data in NC file
		var.put.nc(nc, pkg.env$time_var_name, as.numeric(times)/86400) #convert to days since 1970-01-01
		var.put.nc(nc, pkg.env$lat_coord_var_name, lats)
		var.put.nc(nc, pkg.env$lon_coord_var_name, lons)
		
		if(!is.na(alts[1])){
			var.put.nc(nc, pkg.env$alt_coord_var_name, alts)
		}
		var.put.nc(nc, pkg.env$dsg_timeseries_id, instance_names)
		
		put_data_in_nc(nc, nt, n, data_name, data, alts)
		
		close.nc(nc)
		
		return(nc_file)
	}
}

put_data_in_nc <- function(nc, nt, n, data_name, data, alts=NA) {
	#Add coordinates
	if(!is.na(alts[1])){
	  coordinates <- paste(pkg.env$time_var_name,
	                       pkg.env$lat_coord_var_name,
	                       pkg.env$lon_coord_var_name,
	                       pkg.env$alt_coord_var_name)
	} else {
	  coordinates <- paste(pkg.env$time_var_name,
	                       pkg.env$lat_coord_var_name,
	                       pkg.env$lon_coord_var_name)
	}
	  att.put.nc(nc, data_name, 'coordinates', "NC_CHAR", coordinates)

		att.put.nc(nc, data_name, 'coordinates', "NC_CHAR", coordinates)
		
	if ( nt * n < 100000 ) {
		var.put.nc(nc, data_name, as.matrix(data))
	} else {
		for ( st in 1:n ) {
			var.put.nc(nc, data_name, as.matrix(data[,st]), start=c(1, st), count=c(nt, 1))
		}
	}
}
