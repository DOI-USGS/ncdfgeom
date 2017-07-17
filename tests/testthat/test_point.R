library(ncdf4)

context("NCDF SG point tests")

test_that("Point_timeSeries", {
  expect_error(ToNCDFSG("test"),regexp = "Did not find supported spatial data.")

  multipointData <- readRDS("data/pointData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc<-nc_open(nc_file)

  expect_equal(nc$dim$instance$vals,
               c(1))

  expect_equal(as.numeric(ncvar_get(nc,'y')),
               as.numeric(multipointData@coords[,2]))

  expect_equal(as.numeric(ncvar_get(nc,'x')),
               as.numeric(multipointData@coords[,1]))

  expect_equal(ncatt_get(nc,varid="y","cf_role")$value,
               "geometry_y_node")
  expect_equal(ncatt_get(nc,varid="x","cf_role")$value,
               "geometry_x_node")

  expect_equivalent(ncatt_get(nc,varid=0,"Conventions")$value,
                    "CF-1.8")

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "point")

  expect_equivalent(ncatt_get(nc,varid="y","standard_name")$value,
                    "latitude")
  expect_equivalent(ncatt_get(nc,varid="x","standard_name")$value,
                    "longitude")

  returnPointData<-FromNCDFSG(nc_file)
  expect_equal(as.numeric(multipointData@coords), as.numeric(returnPointData@coords))
  expect_equal(as.numeric(multipointData@bbox), as.numeric(returnPointData@bbox))
})

test_that("multiPoint_timeSeries", {
  multipointData <- readRDS("data/multipointData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc<-nc_open(nc_file)

  expect_equal(nc$dim$instance$vals,c(1))

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "multipoint")

  expect_equal(as.numeric(ncvar_get(nc,'y')),
               as.numeric(multipointData@coords[,2]))

  expect_equal(as.numeric(ncvar_get(nc,'x')),
               as.numeric(multipointData@coords[,1]))

  expect_error(FromNCDFSG(nc_file), "reading multipoint is not supported yet.")
})

test_that("point lat lon", {
  multipointData <- readRDS("data/multipointData.rds")
  lat<-multipointData@coords[,2]
  lon<-multipointData@coords[,1]
  nc_file <- ToNCDFSG(nc_file=tempfile(), lons = lon, lats = lat)
  nc<-nc_open(nc_file)

  expect_equal(nc$dim$instance$vals,
               c(1,2,3,4))

  expect_equal(as.numeric(ncvar_get(nc,'y')),
               as.numeric(multipointData@coords[,2]))

  expect_equal(as.numeric(ncvar_get(nc,'x')),
               as.numeric(multipointData@coords[,1]))

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "point")

  returnPointData<-FromNCDFSG(nc_file)
  expect_equal(as.numeric(multipointData@coords), as.numeric(returnPointData@coords))
  expect_equal(as.numeric(multipointData@bbox), as.numeric(returnPointData@bbox))
})
