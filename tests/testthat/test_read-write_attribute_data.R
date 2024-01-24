
context("instance attributes")

test_that("A dataframe can be round tripped to netCDF.", {
  testthat::skip_if_not(require("ncdf4"))
  dataFrame <- sf::st_set_geometry(sf::read_sf("data/NHDLine/NHDLine.shp"), NULL)
  units<-c("unitless","date","unitless","unitless","unitless","km","unitless","unitless","unknown")
  nc_file <- write_attribute_data(nc_file=tempfile(), att_data = dataFrame, instance_dim_name = "instance", units = units)
  nc <- nc_open(nc_file)
  
  expect_equal(as.character(ncvar_get(nc, nc$var$COMID)), as.character(dataFrame$COMID))
  expect_equal(as.character(ncvar_get(nc, nc$var$FDATE)), as.character(dataFrame$FDATE))
  expect_equal(as.numeric(ncvar_get(nc, nc$var$SHAPE_LENG)), as.numeric(dataFrame$SHAPE_LENG))
	expect_equal(ncatt_get(nc, nc$var$LENGTHKM, "units")$value, "km")
	expect_equal(ncatt_get(nc, nc$var$SHAPE_LENG, "units")$value, "unknown")
  returnDataFrame <- read_attribute_data(nc_file, "instance")
  i <- sapply(dataFrame, is, class2 = "Date")
  dataFrame[i] <- lapply(dataFrame[i], as.character)
  expect_equal(names(dataFrame), names(returnDataFrame)[2:10])
  for(name in names(dataFrame)) {
  	expect_equal(class(dataFrame[name][[1]]), class(returnDataFrame[name][[1]]))
  }
  expect_error(read_attribute_data(nc_file, "garbage"), "The instance dimension was not found in the provided NetCDF object.")
  })

test_that("instance data can be added to an existing netcdf file.", {
  testthat::skip_if_not(require("ncdf4"))
	hucPolygons <- read_sf(system.file('extdata','example_huc_eta.json', package = 'ncdfgeom'))
	hucPolygons <- st_set_geometry(hucPolygons, NULL)
	outFile <- tempfile()
	c <- file.copy(system.file('extdata','hucDemo/example_huc_eta.nc', package = 'ncdfgeom'), outFile)
	nc_file <- write_attribute_data(outFile, hucPolygons, "station")
	nc <- nc_open(nc_file)
	expect_true(all(names(hucPolygons) %in% names(nc$var)))
	orig_names <- c("lat", "lon", "station_name", "et")
	expect_true(all(orig_names %in% names(nc$var)))
}) 