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
#'                                           geomData = hucPolygons, 
#'                                           instance_dim_name = "station", 
#'                                           variables = vars$name)
#'huc_poly <- read_geometry(huc_eta_nc)
#'plot(sf::st_geometry(huc_poly))
#'names(huc_poly)
#'
read_geometry = function(nc_file) {
  
  nc <- open.nc(nc_file)
  on.exit(close.nc(nc), add  = TRUE)
  
  checkVals <- check_netcdf(nc_file)
  
  instance_id<-checkVals$instance_id
  instance_dim<-checkVals$instance_dim
  geom_container <- checkVals$geom_container
  variable_list <- checkVals$variable_list
  crs <- checkVals$crs
  
  line<-FALSE 
  poly<-FALSE 
  point<-FALSE
  if(grepl("polygon", geom_container$geom_type)) { poly<-TRUE
  } else if(grepl("line", geom_container$geom_type)) { line<-TRUE
  } else point <- TRUE
  
  xCoords <- c(var.get.nc(nc, geom_container$x))
  yCoords <- c(var.get.nc(nc, geom_container$y))
  
  if(length(crs) == 0) {
    prj <- "+proj=longlat +datum=WGS84"
  } else {
    prj <- ncmeta::nc_gm_to_prj(crs)
  }
  
  if(point) {
    point_data <- data.frame(x = xCoords, y = yCoords)
    data_frame <- read_attribute_data(nc_file, instance_dim)
    if(nrow(data_frame) != nrow(point_data)) {
      stop("Reading multipoint is not supported yet.")
      # This is where handling for multipoint would go.
    }
    return(st_as_sf(cbind(point_data, data_frame), crs = st_crs(prj), coords = c("x", "y")))
  } else {
    node_count <- c(var.get.nc(nc, geom_container$node_count))
    
    if(length(geom_container$part_node_count) > 0) {
      part_node_count <- var.get.nc(nc, geom_container$part_node_count)
    } else {
      part_node_count <- node_count
    }
    if(length(geom_container$part_type) > 0) {
      part_type <- var.get.nc(nc, geom_container$part_type)
    } else {
      part_type <- rep(pkg.env$multi_val, length(part_node_count))
    }
    
    node_start <- 1
    geom_node_stop <- 0
    pInd <- 1
    Srl <- list()
    Multi <- FALSE
    for(geom in 1:length(node_count)) {
      multi <- FALSE
      geom_node_stop <- geom_node_stop + node_count[geom]
      
      srl <- list()
      
      p <- 0
      
      while(node_start < geom_node_stop) {
        
        p <- p + 1
        
        part_node_stop <- node_start + part_node_count[pInd] - 1
        
        coords <- list(matrix(c(xCoords[node_start:part_node_stop],yCoords[node_start:part_node_stop]), ncol=2))
        
        dimnames(coords[[1]]) <- list(NULL, c(pkg.env$x_nodes, pkg.env$y_nodes))
        
        # Assume not multi to start
        if(part_type[pInd] == pkg.env$hole_val) { 
          p <- p - 1
          srl[[p]] <- append(srl[[p]], coords)
        } else { 
          if(p == 2) {
            multi <- TRUE
            srl <- append(srl, list(coords))
          } else if(p > 2) {
            srl <- append(srl, list(coords))
          } else {
            srl <- append(srl, list(coords))
          }
        }
        
        node_start <- node_start + part_node_count[pInd] 
        pInd <- pInd + 1
      }
      if(multi) {
        Multi <- TRUE
        if(poly) {
          Srl <- append(Srl, list(st_multipolygon(srl)))
        }  else if(line) {
          if(pInd > 1) {
            srl <- unlist(srl, recursive = FALSE)
            Srl <- append(Srl, list(st_multilinestring(srl)))
          } else {
            Srl <- append(Srl, list(st_linestring(srl)))
          }
        }
      } else {
        
        srl <- unlist(srl, recursive = FALSE)
        
        if(poly) {
          Srl <- append(Srl, list(st_polygon(srl)))
        } else if(line) {
          Srl <- append(Srl, list(st_linestring(srl[[1]])))
        }
      }
    }
    dataFrame <- read_attribute_data(nc_file, instance_dim)
    
    for(varName in names(dataFrame)) {
      if(!varName %in% variable_list) {
        dataFrame[varName] <- NULL
      }
    }
    
    return(st_sf(geom = st_sfc(Srl, crs = st_crs(prj)), check_ring_dir = FALSE, dataFrame, 
                 stringsAsFactors = FALSE, agr = "constant", sfc_last = TRUE))
    
  }
}
