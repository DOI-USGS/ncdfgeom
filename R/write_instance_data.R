#'@title Put attribute / instance data in a NetCDF-CF File
#'
#'@param ncFile A string file path to the nc file to be created. It must already have an instance dimension.
#'@param attData A \code{data.frame} as included in a spatial dataFrame.
#'@param instanceDimName A string to name the instance dimension. Defaults to "instance"
#'@param units A character \code{vector} with units for each column of attData. Default is "unknown" for all.
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
write_instance_data <- function(ncFile, attData, instanceDimName = "instance", units = rep("unknown", ncol(attData)), ...) {
	
	n <- nrow(attData)
	
	instance_dim <- ncdim_def(instanceDimName, '', 1:n, create_dimvar=FALSE)
	
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
	
	nc <- nc_create(filename = ncFile, vars = vars, ...)
	
	nc_close(nc)
	
	nc <- nc_open(ncFile, write = TRUE)
	
	if(!is.null(attData)) {
		for(colName in names(attData)) {
			ncvar_put(nc = nc, varid = colName, vals = attData[colName][[1]])
		}
	}
	
	nc_close(nc)
	
	return(ncFile)
}
