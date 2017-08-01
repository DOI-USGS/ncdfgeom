#'@title Get Dataframe from NetCDF-DSG File
#'
#'
#'@param nc A open ncdf4 object.
#'@param instance_dim The NetCDF instance/station dimension.
#'
#'@description
#'Gets instance data from a NetCDF-DSG file and returns it in a \code{data.frame}.
#'This function is intended as a convenience to be used within workflows where
#'the netCDF file is already open and well understood.
#'
#'@export
#'
read_instance_data <- function(nc, instance_dim) {
	
	if(!any(grepl(instance_dim, names(nc$dim)))) {
		stop("The instance dimension was not found in the provided NetCDF object.")
	}
	
  dataFrame <- as.data.frame(list(id = 1:nc$dim[instance_dim][[1]]$len))
  
  for(var in nc$var) {
    if(var$ndims==1 && grepl(var$dim[[1]]$name, instance_dim)) {
      dataFrame[var$name] <- c(ncvar_get(nc, var$name))
    } else if(grepl(var$prec, paste0("^char$")) &&
              (grepl(var$dim[[1]]$name, instance_dim) ||
               grepl(var$dim[[2]]$name, instance_dim)))
      dataFrame[var$name] <- c(ncvar_get(nc, var$name))
  }
  
  dataFrame[] <- lapply(dataFrame, make.true.NA)
  
  return(dataFrame)
}

# found here: http://stackoverflow.com/questions/26220913/replace-na-with-na
make.true.NA <- function(x) if(is.character(x) || is.factor(x)) {
	is.na(x) <- x=="NA"; x } else {
		x }