#' @title Write attribute data to NetCDF-CF
#'
#' @param nc_file \code{character} file path to the nc file to be created. 
#' If adding to a file, it must already have the named instance dimension.
#'@param attData \code{data.frame} with instances as rows and attributes as rows.  
#'@param instance_dim_name \code{character} name for the instance dimension. Defaults to "instance"
#'@param units \code{character} vector with units for each column of attData. Defaults to "unknown" for all.
#'@param ... additional arguments to be passed on to \code{nc_create}.
#'
#'@description
#'Creates a NetCDF file with an instance dimension, and any attributes from a data frame. 
#'Use to create the start of a NetCDF-DSG file. One character length dimension is created
#'long enough to contain the longest provided character string.
#'This function does not implement any CF convention attributes or standard names.
#'Any columns of class date will be converted to character.
#'
#'@importFrom ncdf4 nc_open ncvar_add nc_create nc_close ncvar_def ncvar_put ncdim_def
#'@importFrom methods is
#'
#'@export
#'
#' @examples 
#' sample_data <- sf::st_set_geometry(sf::read_sf(system.file("shape/nc.shp", 
#'                                                            package = "sf")), 
#'                                    NULL)
#' example_file <-write_attribute_data(tempfile(), sample_data,
#'                                     units = rep("unknown", ncol(sample_data)))
#' 
#' ncdump <- system(paste("ncdump -h", example_file), intern = TRUE)
#' cat(ncdump ,sep = "\n")
#' 
write_attribute_data <- function(nc_file, attData, instance_dim_name = "instance", units = rep("unknown", ncol(attData)), ...) {
	
	n <- nrow(attData)
	
	instance_dim <- ncdim_def(instance_dim_name, '', 1:n, create_dimvar=FALSE)
	
	vars<-list()
	types <- list(numeric="double", integer = "integer", character="char")
	
	# Convert any dates to character. This could be improved later.
	i <- sapply(attData, is, class2 = "Date")
	attData[i] <- lapply(attData[i], as.character)
	
	charDimLen<-0
	for(colName in names(attData)) {
		if(grepl(class(attData[colName][[1]]), "character")) {
			charDimLen<-max(sapply(attData[colName][[1]], nchar, keepNA=FALSE), charDimLen)
		}
	}
	if(charDimLen>0) {
		char_dim <- ncdim_def('char', '', 1:charDimLen, create_dimvar=FALSE)
	}
	col <- 1
	for(colName in names(attData)) {
		if(grepl(class(attData[colName][[1]]), "character")) {
			vars <- c(vars, list(ncvar_def(name=colName, units = units[col], dim = list(char_dim, instance_dim),
																		 prec = types[[class(attData[colName][[1]])]])))
		} else if(grepl(class(attData[colName][[1]]), "integer")) {
			vars <- c(vars, list(ncvar_def(name=colName, units = units[col], dim = instance_dim,
																		 prec = types[[class(attData[colName][[1]])]], missval = -9999)))
		} else {
			vars <- c(vars, list(ncvar_def(name=colName, units = units[col], dim = instance_dim,
																		 prec = types[[class(attData[colName][[1]])]], missval = NA)))
		}
		col <- col + 1
	}
	
	if(file.exists(nc_file)) {
		nc <- nc_open(nc_file, write = TRUE)
		for(var in vars) {
			nc <- ncvar_add(nc, var)
		}
	} else {
		nc <- nc_create(filename = nc_file, vars = vars, ...) 
	}
	
	nc_close(nc)
	
	nc <- nc_open(nc_file, write = TRUE)
	
	if(!is.null(attData)) {
		for(colName in names(attData)) {
			ncvar_put(nc = nc, varid = colName, vals = attData[colName][[1]])
		}
	}
	
	nc_close(nc)
	
	return(nc_file)
}
