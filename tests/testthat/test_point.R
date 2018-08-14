library(ncdf4)

context("NCDF SG point tests")

test_that("Point_timeSeries", {
  expect_error(ToNCDFSG("test"),
               regexp = "Did not find supported spatial data.")

  pointData <- get_fixture_data("point")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = pointData)
  nc<-nc_open(nc_file)

  expect_equal(nc$dim$instance$vals,
               c(1))

  expect_equal(as.numeric(ncvar_get(nc,'y')),
               as.numeric(st_coordinates(pointData)[,"Y"]))

  expect_equal(as.numeric(ncvar_get(nc,'x')),
               as.numeric(st_coordinates(pointData)[,"X"]))

  expect_equal(ncatt_get(nc,varid="y","axis")$value,
  						 pkg.env$y_axis)
  expect_equal(ncatt_get(nc,varid="x","axis")$value,
  						 pkg.env$x_axis)

  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,
                    "CF-1.8")

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "point")

  expect_equivalent(ncatt_get(nc,varid="y","standard_name")$value,
                    "latitude")
  expect_equivalent(ncatt_get(nc,varid="x","standard_name")$value,
                    "longitude")

  returnPointData<-FromNCDFSG(nc_file)
  expect_equal(as.numeric(st_coordinates(pointData)), 
  						 as.numeric(st_coordinates(st_as_sf(returnPointData))))
  expect_equal(as.numeric(st_bbox(pointData)), 
  						 as.numeric(st_bbox(st_as_sf(returnPointData))))
})

test_that("multiPoint_timeSeries", {
  multipointData <- get_fixture_data("multipoint")

	expect_error(ToNCDFSG(nc_file=tempfile(), geomData = multipointData),
							 "Multi point not supported yet.")
  
  # expect_error(FromNCDFSG(nc_file), "Reading multipoint is not supported yet.")
})

test_that("point lat lon", {
  multipointData <- get_fixture_data("multipoint")
  lat<-st_coordinates(multipointData)[, "Y"]
  lon<-st_coordinates(multipointData)[, "X"]
  nc_file <- ToNCDFSG(nc_file=tempfile(), lons = lon, lats = lat)
  nc<-nc_open(nc_file)

  expect_equal(nc$dim$instance$vals,
               c(1,2,3,4))

  expect_equal(as.numeric(ncvar_get(nc,'y')),
               as.numeric(st_coordinates(multipointData)[, "Y"]))

  expect_equal(as.numeric(ncvar_get(nc,'x')),
               as.numeric(st_coordinates(multipointData)[, "X"]))

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "point")

  returnPointData<-FromNCDFSG(nc_file)
  expect_equal(as.numeric(st_coordinates(multipointData)[,c("X", "Y")]), 
  						 as.numeric(st_coordinates(st_as_sf(returnPointData))[,c("X", "Y")]))
  expect_equal(as.numeric(st_bbox(multipointData)), 
  						 as.numeric(st_bbox(st_as_sf(returnPointData))))
})
