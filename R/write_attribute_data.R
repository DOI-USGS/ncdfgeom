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
#'@importFrom RNetCDF create.nc open.nc close.nc dim.def.nc dim.inq.nc var.def.nc var.put.nc att.put.nc
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
	
  if(file.exists(nc_file)) {
    nc <- open.nc(nc_file, write = TRUE)
  } else {
    nc <- create.nc(nc_file)
  }
  
	n <- nrow(attData)
	
	instance_dim <- tryCatch({
	  dim.inq.nc(nc, instance_dim_name)$id
	  },
	  error = function(e) {
	    dim <- dim.def.nc(nc, instance_dim_name, n, unlim = FALSE)
	    dim.inq.nc(nc, instance_dim_name)$id
	  })
	
	vars<-list()
	types <- list(numeric="NC_DOUBLE", integer = "NC_INT", character="NC_CHAR")
	
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
		char_dim <- dim.def.nc(nc, "char", charDimLen, unlim = FALSE)
		char_dim <- dim.inq.nc(nc, "char")$id
	}
	col <- 1
	for(colName in names(attData)) {
		if(grepl(class(attData[colName][[1]]), "character")) {
		  dim <- c(char_dim, instance_dim)
		  missval <- ""
		  char <- TRUE
		  attData[colName][[1]][is.na(attData[colName][[1]])] <- ""
		} else if(grepl(class(attData[colName][[1]]), "integer")) {
		  dim <- instance_dim
		  missval <- -9999
		} else {
		  dim <- instance_dim
		  missval <- -9999.999
		}
	  var.def.nc(nc, colName, types[[class(attData[colName][[1]])]], dim)
	  att.put.nc(nc, colName, "units", "NC_CHAR", units[col])
	  att.put.nc(nc, colName, "missing_value", types[[class(attData[colName][[1]])]], missval)
		col <- col + 1
	}

	close.nc(nc)
	
	nc <- open.nc(nc_file, write = TRUE)
	
	if(!is.null(attData)) {
		for(colName in names(attData)) {
			var.put.nc(nc, colName, attData[colName][[1]])
		}
	}
	
	close.nc(nc)
	
	return(nc_file)
}
