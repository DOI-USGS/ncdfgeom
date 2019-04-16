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
#'@importFrom sp Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame CRS Line Lines SpatialLines SpatialLinesDataFrame SpatialPointsDataFrame
#'@importFrom sf st_as_sf
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
    point_data <- matrix(c(xCoords,
                           yCoords), ncol=2)
    dataFrame <- read_attribute_data(nc_file, instance_dim)
    if(nrow(dataFrame) != nrow(point_data)) {
      stop("Reading multipoint is not supported yet.")
      # This is where handling for multipoint would go.
    }
    SPGeom <- SpatialPointsDataFrame(point_data, proj4string = CRS(prj),
                                     data = dataFrame, match.ID = FALSE)
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
    for(geom in 1:length(node_count)) {

      geom_node_stop <- geom_node_stop + node_count[geom]

      srl <- list()

      while(node_start < geom_node_stop) {
        part_node_stop <- node_start + part_node_count[pInd] - 1

        if(part_type[pInd] == pkg.env$hole_val) { hole <- TRUE
        } else { hole <- FALSE }

        coords <- matrix(c(xCoords[node_start:part_node_stop],yCoords[node_start:part_node_stop]),ncol=2)

        if(poly) { tsrl<-Polygon(coords, hole=hole)
        } else if(line) { tsrl<-Line(coords) }

        dimnames(tsrl@coords) <- list(NULL, c(pkg.env$x_nodes, pkg.env$y_nodes))

        srl <- append(srl, tsrl)

        node_start <- node_start + part_node_count[pInd]; pInd <- pInd + 1
      }
      if(poly) {
        Srl <- append(Srl, Polygons(srl, as.character(geom)))
      }  else if(line) {
        Srl <- append(Srl, Lines(srl, as.character(geom)))
      }
    }
    dataFrame <- read_attribute_data(nc_file, instance_dim)

    for(varName in names(dataFrame)) {
      if(!varName %in% variable_list) {
        dataFrame[varName] <- NULL
      }
    }

    if(poly) {
      SPGeom <- SpatialPolygonsDataFrame(SpatialPolygons(Srl, proj4string = CRS(prj)),
                                         dataFrame, match.ID = FALSE)
    } else if(line) {
      SPGeom <- SpatialLinesDataFrame(SpatialLines(Srl, proj4string = CRS(prj)),
                                      dataFrame, match.ID = FALSE)
    }
  }
  return(sf::st_as_sf(SPGeom))
}
