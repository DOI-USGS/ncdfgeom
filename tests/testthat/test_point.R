
context("point")

test_that("Point_timeSeries", {
  expect_error(write_geometry("test"),
               regexp = "Did not find supported spatial data.")

  pointData <- get_fixture_data("point")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = pointData)
  nc<-nc_open(nc_file)

  expect_equal(nc$dim$instance$vals,
               c(1))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$y_nodes)),
               as.numeric(st_coordinates(pointData)[,"Y"]))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$x_nodes)),
               as.numeric(st_coordinates(pointData)[,"X"]))

  expect_equal(ncatt_get(nc,varid=pkg.env$y_nodes,"axis")$value,
  						 pkg.env$y_axis)
  expect_equal(ncatt_get(nc,varid=pkg.env$x_nodes,"axis")$value,
  						 pkg.env$x_axis)

  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,
                    "CF-1.8")

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "point")

  expect_equivalent(ncatt_get(nc,varid=pkg.env$y_nodes,"standard_name")$value,
                    "latitude")
  expect_equivalent(ncatt_get(nc,varid=pkg.env$x_nodes,"standard_name")$value,
                    "longitude")

  returnPointData<-read_geometry(nc_file)
  expect_equal(as.numeric(st_coordinates(pointData)), 
  						 as.numeric(st_coordinates(st_as_sf(returnPointData))))
  expect_equal(as.numeric(st_bbox(pointData)), 
  						 as.numeric(st_bbox(st_as_sf(returnPointData))))
})

test_that("multiPoint_timeSeries", {
  multipointData <- get_fixture_data("multipoint")

	expect_error(write_geometry(nc_file=tempfile(), geomData = multipointData),
							 "Multi point not supported yet.")
  
  # expect_error(read_geometry(nc_file), "Reading multipoint is not supported yet.")
})

test_that("point lat lon", {
  multipointData <- get_fixture_data("multipoint")
  lat<-st_coordinates(multipointData)[, "Y"]
  lon<-st_coordinates(multipointData)[, "X"]
  nc_file <- write_geometry(nc_file=tempfile(), lons = lon, lats = lat)
  nc<-nc_open(nc_file)

  expect_equal(nc$dim$instance$vals,
               c(1,2,3,4))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$y_nodes)),
               as.numeric(st_coordinates(multipointData)[, "Y"]))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$x_nodes)),
               as.numeric(st_coordinates(multipointData)[, "X"]))

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "point")

  returnPointData<-read_geometry(nc_file)
  expect_equal(as.numeric(st_coordinates(multipointData)[,c("X", "Y")]), 
  						 as.numeric(st_coordinates(st_as_sf(returnPointData))[,c("X", "Y")]))
  expect_equal(as.numeric(st_bbox(multipointData)), 
  						 as.numeric(st_bbox(st_as_sf(returnPointData))))
})

test_that("shapefile_point", {
  pointData <- sf::read_sf("data/se_sites/se_sitest.shp")
  nc_file <- write_geometry(nc_file = tempfile(), geomData = pointData)
  nc <- nc_open(nc_file)
  pointData_nogeo <- sf::st_set_geometry(pointData, NULL)
  
  expect_true(all(names(pointData_nogeo) %in% names(nc$var)))
  expect_equal(as.character(pointData$station_nm),as.character(ncvar_get(nc, nc$var$station_nm)))
  expect_equal(length(ncvar_get(nc, nc$var$y)), length(sf::st_coordinates(pointData)[, "Y"]))
  expect_equal(length(ncvar_get(nc, nc$var$x)), length(sf::st_coordinates(pointData)[, "X"]))
  expect_equal(sum(ncvar_get(nc, nc$var$y)), sum(sf::st_coordinates(pointData)[, "Y"]))
  expect_equal(sum(ncvar_get(nc, nc$var$x)), sum(sf::st_coordinates(pointData)[, "X"]))
  expect_equal(as.character(ncvar_get(nc, nc$var$site_no)), pointData$site_no)
  expect_equal(as.numeric(ncvar_get(nc, nc$var$drain_area)), pointData$drain_area)
  
  returnPointData<-read_geometry(nc_file)
  
  expect_equal(as.numeric(sf::st_coordinates(pointData)), as.numeric(sf::st_coordinates(returnPointData)))
  expect_equal(as.numeric(sf::st_bbox(pointData)), as.numeric(sf::st_bbox(returnPointData)))
})

test_that("Point data can be written", {
  dataFrame <- read.csv(system.file("extdata/yahara_alb_attributes.csv", package = "ncdfgeom"))
  nc_file <- write_point_dsg(nc_file=tempfile(), lats = dataFrame$YCOORD, lons = dataFrame$XCOORD, 
                             alts = rep(0, length(dataFrame$XCOORD)), times = as.POSIXct("1970-01-01 00:00:00 UTC", tz = "UTC"),
                             feature_names = dataFrame$ID,
                             data = dataFrame[c("GRIDCODE", "X_COORD", "Y_COORD")],
                             data_units = c("unitless", "m", "m"))
  nc <- nc_open(nc_file)
  
  expect_equal(as.character(ncvar_get(nc, nc$var$feature_name)), as.character(dataFrame$ID))
  expect_equal(as.character(ncvar_get(nc, nc$var$GRIDCODE)), as.character(dataFrame$GRIDCODE))
  expect_equal(as.numeric(ncvar_get(nc, nc$var$Y_COORD)), as.numeric(dataFrame$Y_COORD))
  expect_equal(as.numeric(ncvar_get(nc, nc$var$time))[1], 0)
  expect_equal(ncatt_get(nc,0,"Conventions")$value,"CF-1.7")
  expect_equal(ncatt_get(nc,0,"featureType")$value,"point")
  expect_equal(ncatt_get(nc, nc$var$GRIDCODE, "units")$value, "unitless")
  expect_equal(ncatt_get(nc, nc$var$GRIDCODE, "coordinates")$value, "lat lon alt time")
  expect_equal(ncatt_get(nc, nc$var$Y_COORD, "units")$value, "m")
})
