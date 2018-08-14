#' Get NetCDF-CF Grid Mapping from Projection
#'
#' This function takes a proj4 string and returns a NetCDF-CF projection container as
#' a named list of attributes.
#'
#' https://en.wikibooks.org/wiki/PROJ.4
#' https://trac.osgeo.org/gdal/wiki/NetCDF_ProjectionTestingStatus
#' http://cfconventions.org/cf-conventions/cf-conventions.html#appendix-grid-mappings
#'
#'
#' @param prj A proj.4 string as returned from the sp CRS function.
#'
#' @return A named list containing attributes required for that grid_mapping.
#'
#' @importFrom rgdal CRSargs checkCRSArgs
#' @importFrom sp CRS
#' @export
#'
#' @examples
#' prj <- "+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs"
#' grid_mapping <- get_gridmapping(prj)
#'
get_gridmapping <- function(prj) {
  al <- prepCRS(prj)
  if(is.null(al)) {
    return(list()) } else {
    return(GGFP(al)) }
}

GGFP <- function(al) UseMethod("GGFP")

GGFP.latitude_longitude <- function(al) {
  gm <- c(list(grid_mapping_name = "latitude_longitude"),
          getGeoDatum_gm(al))
}

GGFP.albers_conical_equal_area <- function(al) {
  gm <- c(list(grid_mapping_name = "albers_conical_equal_area"),
       lonCentMer_gm(al),
       latProjOrig_gm(al),
       falseEastNorth_gm(al),
       standPar_gm(al),
       getGeoDatum_gm(al))
}

GGFP.azimuthal_equidistant <- function(al) {
  gm <- c(list(grid_mapping_name = "azimuthal_equidistant"),
          lonProjOrig_gm(al),
          latProjOrig_gm(al),
          falseEastNorth_gm(al),
          getGeoDatum_gm(al))
}

GGFP.lambert_azimuthal_equal_area <- function(al) {
  gm <- c(list(grid_mapping_name = "lambert_azimuthal_equal_area"),
          latProjOrig_gm(al),
          lonProjOrig_gm(al),
          falseEastNorth_gm(al),
          getGeoDatum_gm(al))
}

GGFP.lambert_conformal_conic <- function(al) {
  gm <- c(list(grid_mapping_name = "lambert_conformal_conic"),
                    standPar_gm(al),
                    falseEastNorth_gm(al),
                    latProjOrig_gm(al),
                    lonCentMer_gm(al),
                    getGeoDatum_gm(al))
}

GGFP.lambert_cylindrical_equal_area <- function(al) {
  gm <- c(list(grid_mapping_name = "lambert_cylindrical_equal_area"),
                    lonCentMer_gm(al),
                    oneStandPar_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

GGFP.mercator <- function(al) {
  if(!is.null(al$k)) {
    gm <- c(list(grid_mapping_name = "mercator"),
                      lonProjOrig_gm(al),
                      scaleFactor_gm(al),
                      falseEastNorth_gm(al),
                      getGeoDatum_gm(al))
  } else {
    gm <- c(list(grid_mapping_name = "mercator"),
                      lonProjOrig_gm(al),
                      oneStandPar_gm(al),
                      falseEastNorth_gm(al),
                      getGeoDatum_gm(al))
  }
}

GGFP.oblique_mercator <- function(al) {
  #!!!! Check this one out. the oMerc function is a hack !!!!
  gm <- c(list(grid_mapping_name = "oblique_mercator"),
                    latProjOrig_gm(al),
                    lonProjCent_gm(al),
                    scaleFactor_gm(al),
                    oMerc_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

GGFP.orthographic <- function(al) {
  gm <- c(list(grid_mapping_name = "orthographic"),
                    latProjOrig_gm(al),
                    lonProjOrig_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

# GGFP.polar_stereographic <- function(al) {
#   if(!is.null(al$k)) {
#     gm <- c(list(grid_mapping_name = "polar_stereographic"),
#                       latProjOrig_gm(al),
#                       stVertLon_gm(al),
#                       scaleFactor_gm(al),
#                       falseEastNorth_gm(al),
#                       getGeoDatum_gm(al))
#   } else {
#     gm <- c(list(grid_mapping_name = "polar_stereographic"),
#                       latProjOrig_gm(al),
#                       stVertLon_gm(al),
#                       oneStandPar_gm(al),
#                       falseEastNorth_gm(al),
#                       getGeoDatum_gm(al))
#   }
# }

# GGFP.sinusoidal <- function(al) {
#   gm <- c(list(grid_mapping_name = "sinusoidal"),
#                     lonProjOrig_gm(al),
#                     falseEastNorth_gm(al),
#                     getGeoDatum_gm(al))
# }

GGFP.stereographic <- function(al) {
  gm <- c(list(grid_mapping_name = "stereographic"),
                    latProjOrig_gm(al),
                    lonProjOrig_gm(al),
                    scaleFactor_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

GGFP.transverse_mercator <- function(al) {
  gm <- c(list(grid_mapping_name = "transverse_mercator"),
                    latProjOrig_gm(al),
                    lonProjOrig_gm(al),
                    scaleFactor_gm(al),
                    falseEastNorth_gm(al),
                    getGeoDatum_gm(al))
}

lonCentMer_gm <- function(al) {
  list(longitude_of_central_meridian = as.numeric(al$lon_0))
}

latProjOrig_gm <- function(al) {
  list(latitude_of_projection_origin = as.numeric(al$lat_0))
}

lonProjOrig_gm <- function(al) {
  list(longitude_of_projection_origin = as.numeric(al$lon_0))
}

falseEastNorth_gm <- function(al) {
  list(false_easting = as.numeric(al$x_0),
  false_northing = as.numeric(al$y_0))
}

standPar_gm <- function(al) {
  if(al$lat_1 != al$lat_2) {
    list(standard_parallel = c(as.numeric(al$lat_1), as.numeric(al$lat_2)))
  } else if(al$lat_1 == al$lat_2) {
    list(standard_parallel = as.numeric(al$lat_1))
  }
}

oneStandPar_gm <- function(al) {
  list(standard_parallel = c(as.numeric(al$lat_ts)))
}

getGeoDatum_gm <- function(al) {
  if(!is.null(al$datum) && al$datum == "NAD83") {
    list(semi_major_axis = 6378137,
         inverse_flattening = 298.257222101,
         longitude_of_prime_meridian = 0)
  } else if(!is.null(al$datum) && al$datum == "WGS84") {
    list(semi_major_axis = 6378137,
         inverse_flattening = 298.257223563,
         longitude_of_prime_meridian = 0)
  } else if(!is.null(al$ellps) && 
  					!is.null(al$towgs84) && 
  					al$towgs84 == "0,0,0,0,0,0,0") {
  	list(semi_major_axis = 6378137,
  			 inverse_flattening = 298.257223563,
  			 longitude_of_prime_meridian = 0)
  } else if(!is.null(al$a) && !is.null(al$f) && !is.null(al$pm)) {
    list(semi_major_axis = as.numeric(al$a),
        inverse_flattening = (1/as.numeric(al$f)),
        longitude_of_prime_meridian = as.numeric(al$pm))
  } else if(!is.null(al$a) && !is.null(al$b) && !is.null(al$pm)) {
  	list(semi_major_axis = as.numeric(al$a),
  			 inverse_flattening = (1/as.numeric(al$b)),
  			 longitude_of_prime_meridian = as.numeric(al$pm))
  } else {
  	warning("no datum information found assuming WGS84")
  	list(semi_major_axis = 6378137,
  			 inverse_flattening = 298.257223563,
  			 longitude_of_prime_meridian = 0)
  }
}

scaleFactor_gm <- function(al) {
  list(scale_factor_at_projection_origin = as.numeric(al$k))
}

oMerc_gm <- function(al) {
  list(azimuth_of_central_line = as.numeric(al$alpha))
}

lonProjCent_gm <- function(al) {
  list(longitude_of_projection_origin = as.numeric(al$lonc))
}

prepCRS <- function(prj) {
  if(class(prj) == "CRS") prj <- CRSargs(prj)

  if(!checkCRSArgs(prj)[1][[1]]) {
    warning("not a valid crs, returning an empty crs list")
    return(NULL)
  }

  args <- unique(unlist(strsplit(prj, " ")))

  argList <- list()

  for(arg in args) {
    a <- unlist(strsplit(sub("\\+", "", arg), "="))
    argList[a[1]] <- a[2]
  }

  cf_proj_lookup <- list(aea = "albers_conical_equal_area",
                         aeqd = "azimuthal_equidistant",
                         laea = "lambert_azimuthal_equal_area",
                         lcc = "lambert_conformal_conic",
                         cea = "lambert_cylindrical_equal_area",
                         longlat = "latitude_longitude",
                         merc = "mercator",
                         omerc = "oblique_mercator",
                         ortho = "orthographic",
                         stere = "stereographic",
                         tmerc = "transverse_mercator")

  class(argList) <- cf_proj_lookup[unlist(argList["proj"])][[1]]

  if(!class(argList) %in% cf_proj_lookup) {
    warning("no available mapping to netcdf projection, returning empty crs list")
    return(NULL) } else {
    return(argList) }
}
