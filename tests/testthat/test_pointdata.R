context("NCDF SG pointData tests")

test_that("shapefile_point", {
  pointData <- sf::read_sf("data/se_sites/se_sitest.shp")
  instance_names <- pointData$station_nm
  nc_file <- ToNCDFSG(nc_file = tempfile(), geomData = pointData, instance_names = instance_names)
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
  
  returnPointData<-FromNCDFSG(nc_file)
  
  expect_equal(as.numeric(sf::st_coordinates(pointData)), as.numeric(sf::st_coordinates(returnPointData)))
  expect_equal(as.numeric(sf::st_bbox(pointData)), as.numeric(sf::st_bbox(returnPointData)))
})
