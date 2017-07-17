context("NCDF check NC tests")

test_that("line", {
  lineData <- readRDS("data/lineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  checkVals <- checkNCDF(nc)

  expect_equal(checkVals$instance_id, NULL)
  expect_equal(checkVals$instanceDim, pkg.env$instance_dim_name)
  expect_equal(checkVals$geom_container$geom_type, "line")
  expect_equal(checkVals$geom_container$node_count, pkg.env$node_count_var_name)
  expect_equal(checkVals$geom_container$part_node_count, 0)
  expect_equal(checkVals$geom_container$part_type, 0)
  expect_equal(checkVals$geom_container$x, "x")
  expect_equal(checkVals$geom_container$y, "y")
})

test_that("line", {
  lineData <- readRDS("data/multiLineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  checkVals <- checkNCDF(nc)
  expect_equal(checkVals$geom_container$geom_type, "multiline")
  expect_equal(checkVals$geom_container$node_count, pkg.env$node_count_var_name)
  expect_equal(checkVals$geom_container$part_node_count, pkg.env$part_node_count_var_name)
  expect_equal(checkVals$geom_container$part_type, 0)
})

test_that("multi polygon holes", {
  polygonData <- readRDS("data/multipolygons_holes.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  checkVals <- checkNCDF(nc)

  expect_equal(checkVals$geom_container$geom_type, "multipolygon")
  expect_equal(checkVals$geom_container$node_count, pkg.env$node_count_var_name)
  expect_equal(checkVals$geom_container$part_node_count, pkg.env$part_node_count_var_name)
  expect_equal(checkVals$geom_container$part_type, pkg.env$part_type_var_name)
})

test_that("multi polygon holes", {
  polygonData <- readRDS("data/polygonData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  checkVals <- checkNCDF(nc)

  expect_equal(checkVals$geom_container$geom_type, "polygon")
  expect_equal(checkVals$geom_container$node_count, pkg.env$node_count_var_name)
  expect_equal(checkVals$geom_container$part_node_count, 0)
  expect_equal(checkVals$geom_container$part_type, 0)
})

test_that("basic point works", {
  multipointData <- readRDS("data/pointData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc<-nc_open(nc_file)

  checkVals <- checkNCDF(nc)

  expect_equal(checkVals$instanceDim, pkg.env$instance_dim_name)
})

test_that("a crs gets found correctly", {
  polygonData <- readRDS("data/yahara_shapefile_data.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)
  checkVals <- checkNCDF(nc)
  crs <- list(grid_mapping_name = "albers_conical_equal_area",
              longitude_of_central_meridian = -96,
              latitude_of_projection_origin = 23,
              false_easting = 0.0,
              false_northing = 0.0,
              standard_parallel = c(29.5, 45.5),
              semi_major_axis = 6378137.0,
              inverse_flattening = 298.257223563,
              longitude_of_prime_meridian = 0)
  expect_equal(checkVals$crs, crs)
})

test_that("errors", {
  multipointData <- readRDS("data/pointData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc <- nc_open(nc_file, write = TRUE)
  ncatt_put(nc, nc$var$y, pkg.env$geom_type_attr_name, "garbage")
  nc_close(nc)
  nc<-nc_open(nc_file, write = TRUE)
  expect_error(checkNCDF(nc), "only one geometry container per file supported")

  # nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  # nc <- nc_open(nc_file, write = TRUE)
  # ncatt_put(nc, nc$var$lat, "cf_role", "timeseries_id")
  # expect_error(checkNCDF(nc), 'multiple timeseries id variables were found.')

  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  nc <- nc_open(nc_file, write = TRUE)
  ncatt_put(nc, 0,"Conventions", "garbage")
  expect_warning(checkNCDF(nc), 'File does not advertise CF conventions, unexpected behavior may result.')

  # lineData <- readRDS("data/multiLineData.rds")
  # nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  # nc<-nc_open(nc_file, write = TRUE)
  # ncatt_put(nc, nc$var$instance_name, "node_coordinates", "x y")
  # nc_close(nc)
  # nc<-nc_open(nc_file, write = TRUE)
  # expect_error(checkNCDF(nc), "only one node_coordinates index is supported, this file has more than one.")
  #
  # lineData <- readRDS("data/multiLineData.rds")
  # nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  # nc<-nc_open(nc_file, write = TRUE)
  # ncatt_put(nc, nc$var$x, "contiguous_ragged_dimension", "coordinate_index")
  # nc_close(nc)
  # nc<-nc_open(nc_file, write = TRUE)
  # expect_error(checkNCDF(nc), "only one contiquous ragged dimension index is supported, this file has more than one.")
  #
  # multipointData <- readRDS("data/pointData.rds")
  # nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = multipointData)
  # nc <- nc_open(nc_file, write = TRUE)
  # ncatt_put(nc, nc$var$lat, "standard_name", "garbage")
  # expect_warning(checkNCDF(nc), "instance dimension is being inferred based on an assumption of dimension order of the character instance_id and may not be correct.")
  #
  # nc <- nc_open("data/borked_featureType.nc")
  #
  # expect_warning(checkNCDF(nc), "File does not have a featureType declaration, unexpected behavior may result.")
})
