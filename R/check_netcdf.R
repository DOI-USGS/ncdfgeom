#'@title Check NetCDF-DSG File
#'
#'
#'@param nc A NetCDF path or URL to be opened.
#'
#'@description
#'Introspects a netcdf file and tries to interpret it as a NetCDF-DSG file. Returns a named
#'\code{list} containing \code{instance_id} \code{instance_dim} \code{node_count}
#'\code{part_node_count} \code{part_type} If these values aren't found or aren't applicable,
#'they are returned \code{NULL}.
#'
#'@references
#'https://github.com/twhiteaker/netCDF-CF-simple-geometry
#'
#'@importFrom ncmeta nc_atts nc_axes nc_dim
#'@importFrom stats setNames
#'
#'@noRd
#'
check_netcdf <- function(nc) {

  instance_id<-NULL
  instance_dim<-NULL
  geom_container <- list(geom_type = NULL, node_count = NULL, part_node_count = NULL,
                         part_type = NULL, x = NULL, y = NULL)

  atts <- nc_atts(nc)
  
  # Check important global atts
  if(!grepl('CF',get_att(atts, "NC_GLOBAL", "Conventions")$value)) {
    warning('File does not advertise CF conventions, unexpected behavior may result.')}

  geom_container_var<-find_var_by_att(atts, pkg.env$geom_type_attr_name)

  if(length(geom_container_var) > 1) {
    stop("only one geometry container per file supported")
  } else if(length(geom_container_var) == 0) {
    stop("Didn't find a geometry type attribute, nothing to do.")
  } else {
    geom_container_var <- geom_container_var[[1]]

    geom_container$geom_type <- as.character(get_att(atts, geom_container_var, pkg.env$geom_type_attr_name)$value)

    geom_container$node_count <- as.character(get_att(atts, geom_container_var, pkg.env$node_count_attr_name)$value)

    geom_container$part_node_count <- as.character(get_att(atts, geom_container_var, pkg.env$part_node_count_attr_name)$value)

    geom_container$part_type <- as.character(get_att(atts, geom_container_var, pkg.env$part_type_attr_name)$value)

    node_coordinates <- strsplit(get_att(atts, geom_container_var, pkg.env$node_coordinates)$value[[1]], " ")[[1]]

    for(v in node_coordinates) {
      att <- get_att(atts, v, "axis")
      if(nrow(att) != 0) {
        if(att$value == pkg.env$x_axis) {
          geom_container$x <- v
        } else if(att$value == pkg.env$y_axis) {
          geom_container$y <- v
        } else {
          stop(paste("unexpected axis attribute", pkg.env$x_axis, "and", pkg.env$y_axis, "are allowed."))
        }
      }
    }

    variable_list <- find_var_by_att(atts, pkg.env$geometry_container_att_name, geom_container_var)

  }

  # Look for variable with the timeseries_id in it.
  instance_id<-list()
  instance_id<-append(instance_id, find_var_by_att(atts, 'cf_role', 'timeseries_id'))

  instance_id<-unlist(unique(instance_id))
  if(length(instance_id)>1) { stop('multiple timeseries id variables were found.') }

  if(length(geom_container$node_count) == 0) {
    instance_dim <- nc_dim(nc, nc_axes(nc, geom_container$x)$dimension)$name
  } else {
    instance_dim <- nc_dim(nc, nc_axes(nc, geom_container$node_count)$dimension)$name
  }

  crs_referents <- c(find_var_by_att(atts, "grid_mapping"))

  crs <- list()
  if(length(crs_referents) > 0) {
    for(crs_referent in crs_referents) {
        crs <- c(crs, get_att(atts, crs_referent, "grid_mapping")$value)
    }
    if(length(unique(crs)) > 1) {
      warning("Only one crs is supported, more than one was found, may be handling projections wrong.")
      crs <- crs[1]
    }
    
    crs <- get_att(atts, crs[[1]])
    crs <- stats::setNames(crs$value, crs$name)
  }

  return(list(instance_id = instance_id,
              instance_dim = instance_dim,
              geom_container = geom_container,
              variable_list = variable_list,
              crs = crs))
}

get_att <- function(atts, var, att = NULL) {
  
  name <- value <- variable <- NULL
  
  if(is.null(att)) {
    filter(atts, variable == var)
  } else {
    filter(atts, variable == var, name == att)
  }
}

find_var_by_att <- function(atts, attribute, search_value = ".*", strict = TRUE) {
  
  name <- value <- NULL
  
  if(strict) search_value <-paste0("^", search_value, "$")
  
  filter(atts, name == attribute, grepl(search_value, value))$variable
}
