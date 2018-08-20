#' Get projection from NetCDF-CF Grid Mapping
#'
#' Takes NetCDF-CF grid mapping attributes and returns
#' a proj4 string.
#'
#' The WGS84 datum is used as a default if one os not provided
#' in the grid mapping.
#'
#' If only a semi_major axis is provided, a sperical earth is assumed.
#'
#' @param gm A list of attributes of the grid mapping variable
#' as returned by ncdf or ncdf4's get attributes functions.
#'
#' @return A proj4 string.
#'
#' @references 
#' \enumerate{
#'   \item \url{https://en.wikibooks.org/wiki/PROJ.4}
#'   \item \url{https://trac.osgeo.org/gdal/wiki/NetCDF_ProjectionTestingStatus}
#'   \item \url{http://cfconventions.org/cf-conventions/cf-conventions.html#appendix-grid-mappings}
#' }
#' 
#' @export
#'
#' @examples
#'
#' crs <- list(grid_mapping_name="latitude_longitude",
#'             longitude_of_prime_meridian = 0,
#'             semi_major_axis = 6378137,
#'             inverse_flattening = 298)
#' get_prj(crs)
#' 
#'
get_prj <- function(gm) {
  class(gm) <- gm$grid_mapping_name
  GPFN(gm)
}

GPFN <- function(gm) UseMethod("GPFN")

GPFN.albers_conical_equal_area <- function(gm) {
  projargs <- paste("+proj=aea",
                    standPar(gm),
                    falseEastNorth(gm),
                    latProjOrig(gm),
                    lonCentMer(gm),
                    getGeoDatum(gm))
}

GPFN.azimuthal_equidistant <- function(gm) {
  projargs <- paste("+proj=aeqd",
                    latProjOrig(gm),
                    lonProjOrig(gm),
                    falseEastNorth(gm),
                    getGeoDatum(gm))
}
# GPFN.geostationary <- function(gm) {
#   #+proj=geos +lon_0=0 +h=-0 +x_0=0 +y_0=0 +no_defs
# projargs <- paste("+proj=geos",
#                   latProjOrig(gm),
#                   lonProjOrig(gm),
#                   # persHeight(gm),
#                   falseEastNorth(gm),
#                   getGeoDatum(gm))
# # Fixed angle and sweep angle axes?
# }
GPFN.lambert_azimuthal_equal_area <- function(gm) {
  projargs <- paste("+proj=laea",
                    latProjOrig(gm),
                    lonProjOrig(gm),
                    falseEastNorth(gm),
                    getGeoDatum(gm))
}

GPFN.lambert_conformal_conic <- function(gm) {
  projargs <- paste("+proj=lcc",
                    standPar(gm),
                    falseEastNorth(gm),
                    latProjOrig(gm),
                    lonCentMer(gm),
                    getGeoDatum(gm))
}

GPFN.lambert_cylindrical_equal_area <- function(gm) {
  projargs <- paste("+proj=cea",
                    lonCentMer(gm),
                    oneStandPar(gm),
                    falseEastNorth(gm),
                    getGeoDatum(gm))
}

GPFN.latitude_longitude <- function(gm) {
  prj <- paste0("+proj=longlat ", getGeoDatum(gm))
}
GPFN.mercator <- function(gm) {
  if(!is.null(gm$scale_factor_at_projection_origin)) {
    projargs <- paste("+proj=merc",
                      lonProjOrig(gm),
                      scaleFactor(gm),
                      falseEastNorth(gm),
                      getGeoDatum(gm))
  } else {
    projargs <- paste("+proj=merc",
                      lonProjOrig(gm),
                      oneStandPar(gm),
                      falseEastNorth(gm),
                      getGeoDatum(gm))
  }
}
GPFN.oblique_mercator <- function(gm) {
  #!!!! Check this one out. the oMerc function is a hack !!!!
  projargs <- paste("+proj=omerc",
                    latProjOrig(gm),
                    lonProjCent(gm),
                    scaleFactor(gm),
                    oMerc(gm),
                    falseEastNorth(gm),
                    getGeoDatum(gm))
}

GPFN.orthographic <- function(gm) {
  projargs <- paste("+proj=ortho",
                    latProjOrig(gm),
                    lonProjOrig(gm),
                    falseEastNorth(gm),
                    getGeoDatum(gm))
}

GPFN.polar_stereographic <- function(gm) {
  if(!is.null(gm$scale_factor_at_projection_origin)) {
    projargs <- paste("+proj=stere",
                      latProjOrig(gm),
                      stVertLon(gm),
                      scaleFactor(gm),
                      falseEastNorth(gm),
                      getGeoDatum(gm))
  } else {
    projargs <- paste("+proj=stere",
                      latProjOrig(gm),
                      stVertLon(gm),
                      oneStandPar(gm),
                      falseEastNorth(gm),
                      getGeoDatum(gm))
  }
}
# GPFN.rotated_latitude_longitude <- function(gm) {
#   # not supported?
# }
#
GPFN.sinusoidal <- function(gm) {
projargs <- paste("+proj=sinu",
                  lonProjOrig(gm),
                  falseEastNorth(gm),
                  getGeoDatum(gm))
}
#
GPFN.stereographic <- function(gm) {
  projargs <- paste("+proj=stere",
                    latProjOrig(gm),
                    lonProjOrig(gm),
                    scaleFactor(gm),
                    falseEastNorth(gm),
                    getGeoDatum(gm))
}

GPFN.transverse_mercator <- function(gm) {
  projargs <- paste("+proj=tmerc",
                    latProjOrig(gm),
                    lonProjOrig(gm),
                    scaleFactor(gm),
                    falseEastNorth(gm),
                    getGeoDatum(gm))
}
#
# GPFN.vertical_perspective <- function(gm) {
#   #"+proj=nsper +h=1"
# Not supported?
# }

getGeoDatum <- function(gm) {

  if(is.null(gm$longitude_of_prime_meridian)) {
    warning("Didn't find a longitude of prime meridian for datum, assuming 0.")
    gm$longitude_of_prime_meridian <- 0
  }

  if(is.null(gm$semi_major_axis)) {
    warning("Didn't find a semi major axis for datum, assuming WGS84 6378137.0 meters")
    gm$semi_major_axis <- 6378137.0

    if(is.null(gm$inverse_flattening) && is.null(gm$semi_minor_axis)) {
      warning("Didn't find an inverse flattening value, assuming WGS84 298.257223563")
      gm$inverse_flattening <- 298.257223563
    }
  }

  if(!is.null(gm$inverse_flattening)) {
    geoDatum <- paste0("+a=", gm$semi_major_axis,
                       " +f=", (1/gm$inverse_flattening),
                       " +pm=", gm$longitude_of_prime_meridian,
                       " +no_defs")
  } else if(!is.null(gm$semi_minor_axis)) {
    geoDatum <- paste0("+a=", gm$semi_major_axis,
                       " +b=", gm$semi_minor_axis,
                       " +pm=", gm$longitude_of_prime_meridian,
                       " +no_defs")
  } else {
    geoDatum <- paste0("+a=", gm$semi_major_axis,
                       " +b=", gm$semi_major_axis,
                       " +pm=", gm$longitude_of_prime_meridian,
                       " +no_defs")
  }
  return(geoDatum)
}

standPar <- function(gm) {
  if(length(gm$standard_parallel)==1) {
    gm$standard_parallel <-
      c(gm$standard_parallel,
        gm$standard_parallel)
  }
  outString <- paste0("+lat_1=", gm$standard_parallel[1],
                      " +lat_2=", gm$standard_parallel[2])
}

oneStandPar <- function(gm) {
  outString <- paste0("+lat_ts=", gm$standard_parallel[1])
}

falseEastNorth <- function(gm) {
  options(scipen=2)
	outString <- paste0("+x_0=", gm$false_easting,
											" +y_0=", gm$false_northing,
											" +units=m")
}

latProjOrig <- function(gm) {
  outString <- paste0("+lat_0=", gm$latitude_of_projection_origin)
}

lonCentMer <- function(gm) {
  outString <- paste0("+lon_0=", gm$longitude_of_central_meridian)
}

lonProjOrig <- function(gm) {
  outString <- paste0("+lon_0=", gm$longitude_of_projection_origin)
}

stVertLon <- function(gm) {
  outString <- paste0("+lon_0=", gm$straight_vertical_longitude_from_pole)
}

scaleFactor <- function(gm) {
  outString <- paste0("+k=", gm$scale_factor_at_projection_origin)
}

oMerc <- function(gm) {
  outString <- paste0("+alpha=", gm$azimuth_of_central_line,
                      " +gamma=", gm$azimuth_of_central_line,
                      " +no_uoff")
}

lonProjCent <- function(gm) {
  outString <- paste0("+lonc=", gm$longitude_of_projection_origin)
}

