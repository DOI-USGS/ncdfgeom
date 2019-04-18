#'@title Read NetCDF-CF spatial geometries
#'
#'@param nc_file character file path to the nc file to be read.
#'
#'@description
#'Attemps to convert a NetCDF-CF DSG Simple Geometry file into a sf data.frame.
#'
#'@references
#'http://cfconventions.org/index.html
#'
#'@importFrom RNetCDF open.nc var.get.nc close.nc
#'@importFrom sf st_sf st_sfc st_linestring st_polygon st_multipolygon st_multilinestring st_crs
#'
#'@return sf \code{data.frame} containing spatial geometry of type found in the NetCDF-CF DSG file.
#'
#'@export
#'
#'@examples
#'huc_eta_nc <- tempfile()
#'file.copy(system.file('extdata','example_huc_eta.nc', package = 'ncdfgeom'), 
#'          huc_eta_nc, overwrite = TRUE)
#'          
#'vars <- ncmeta::nc_vars(huc_eta_nc)
#'
#'hucPolygons <- sf::read_sf(system.file('extdata','example_huc_eta.json', package = 'ncdfgeom'))
#'plot(sf::st_geometry(hucPolygons))
#'names(hucPolygons)
#'
#'hucPolygons_nc <- ncdfgeom::write_geometry(nc_file=huc_eta_nc, 
#'                                           geom_data = hucPolygons, 
#'                                           instance_dim_name = "station", 
#'                                           variables = vars$name)
#'huc_poly <- read_geometry(huc_eta_nc)
#'plot(sf::st_geometry(huc_poly))
#'names(huc_poly)
#'
read_geometry = function(nc_file) {
  
  nc <- open.nc(nc_file)
  on.exit(close.nc(nc), add  = TRUE)
  
  nc_props <- check_netcdf(nc)
  
  return(read_geom_data(nc_props, nc))
}

#' @noRd
read_geom_data <- function(nc_props, nc) UseMethod("read_geom_data")

#' @name read_geom_data
read_geom_data.point <- function(nc_props, nc) {
  coord <- get_coords(nc, nc_props)
  prj <- get_prj_nc(nc_props)
  
  point_data <- data.frame(x = coord$x, y = coord$y)
  data_frame <- read_attribute_data(nc, nc_props$instance_dim)
  if(nrow(data_frame) != nrow(point_data)) {
    stop("Reading multipoint is not supported yet.")
    # This is where handling for multipoint would go.
  }
  return(st_as_sf(cbind(point_data, data_frame), crs = st_crs(prj), coords = c("x", "y")))
}

#' @name read_geom_data
read_geom_data.line <- function(nc_props, nc) {
  return(parse_geom(nc_props, nc))
}

#' @name read_geom_data
read_geom_data.polygon <- function(nc_props, nc) {
  return(parse_geom(nc_props, nc))
}
  
get_coords <- function(nc, nc_props) {
  list(x = c(var.get.nc(nc, nc_props$geom_container$x)),
       y = c(var.get.nc(nc, nc_props$geom_container$y)))
}

get_prj_nc <- function(nc_props) {
  if(length(nc_props$crs) == 0) {
    warning("no crs found, using WGS84")
    prj <- "+proj=longlat +datum=WGS84"
  } else {
    prj <- ncmeta::nc_gm_to_prj(nc_props$crs)
  }
}

populate_nc_props <- function(nc_props, nc) {
  nc_props$geom_container$node_count <- c(var.get.nc(nc, nc_props$geom_container$node_count))
  
  if(length(nc_props$geom_container$part_node_count) > 0) {
    nc_props$geom_container$part_node_count <- var.get.nc(nc, nc_props$geom_container$part_node_count)
  } else {
    nc_props$geom_container$part_node_count <- nc_props$geom_container$node_count
  }
  if(length(nc_props$geom_container$part_type) > 0) {
    nc_props$geom_container$part_type <- var.get.nc(nc, nc_props$geom_container$part_type)
  } else {
    nc_props$geom_container$part_type <- rep(pkg.env$multi_val, length(nc_props$geom_container$part_node_count))
  }
  return(nc_props)
}

parse_geom <- function(nc_props, nc) {
  coord <- get_coords(nc, nc_props)
  prj <- get_prj_nc(nc_props)
  
  nc_props <- populate_nc_props(nc_props, nc)
  
  poly <- class(nc_props) == "polygon"
  
  node_start <- 1
  geom_node_stop <- 0
  p_ind <- 1
  f_list <- list()
  multi_geometry <- FALSE
  for(geom in 1:length(nc_props$geom_container$node_count)) {
    multi_g <- FALSE
    geom_node_stop <- geom_node_stop + nc_props$geom_container$node_count[geom]
    
    p_list <- list()
    p <- 0
    while(node_start < geom_node_stop) {
      
      p <- p + 1
      
      part_node_stop <- node_start + nc_props$geom_container$part_node_count[p_ind] - 1
      
      coords <- list(matrix(c(coord$x[node_start:part_node_stop], 
                              coord$y[node_start:part_node_stop]), 
                            ncol = 2))
      
      dimnames(coords[[1]]) <- list(NULL, c(pkg.env$x_nodes, pkg.env$y_nodes))
      
      # Assume not multi to start
      if(nc_props$geom_container$part_type[p_ind] == pkg.env$hole_val) { 
        p <- p - 1
        p_list[[p]] <- append(p_list[[p]], coords)
      } else { 
        if(p == 2) {
          multi_g <- TRUE
          p_list <- append(p_list, list(coords))
        } else if(p > 2) {
          p_list <- append(p_list, list(coords))
        } else {
          p_list <- append(p_list, list(coords))
        }
      }
      
      node_start <- node_start + nc_props$geom_container$part_node_count[p_ind] 
      p_ind <- p_ind + 1
    }
    if(multi_g) {
      multi_geometry <- TRUE
      if(poly) {
        f_list <- append(f_list, list(st_multipolygon(p_list)))
      } else {
        if(p_ind > 1) {
          p_list <- unlist(p_list, recursive = FALSE)
          f_list <- append(f_list, list(st_multilinestring(p_list)))
        } else {
          f_list <- append(f_list, list(st_linestring(p_list)))
        }
      }
    } else {
      
      p_list <- unlist(p_list, recursive = FALSE)
      
      if(poly) {
        f_list <- append(f_list, list(st_polygon(p_list)))
      } else {
        f_list <- append(f_list, list(st_linestring(p_list[[1]])))
      }
    }
  }
  df <- read_attribute_data(nc, nc_props$instance_dim)
  
  for(var_name in names(df)) {
    if(!var_name %in% nc_props$variable_list) {
      df[var_name] <- NULL
    }
  }
  
  return(st_sf(geom = st_sfc(f_list, crs = st_crs(prj)), check_ring_dir = FALSE, df, 
               stringsAsFactors = FALSE, agr = "constant", sfc_last = TRUE))
}