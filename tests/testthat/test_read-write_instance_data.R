library("ncdf4")
library("sp")

context("Test adding instance data to an new or existing netcdf file.")

test_that("A dataframe can be round tripped to netCDF.", {
  dataFrame <- readRDS("data/NHDline_data.rds")@data
  units<-c("unitless","date","unitless","unitless","unitless","km","unitless","unitless","unknown")
  nc_file <- write_instance_data(nc_file=tempfile(), attData = dataFrame, instanceDimName = "instance", units = units)
  nc <- nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(as.character(ncvar_get(nc, nc$var$COMID)), as.character(dataFrame$COMID))
  expect_equal(as.character(ncvar_get(nc, nc$var$FDATE)), as.character(dataFrame$FDATE))
  expect_equal(as.numeric(ncvar_get(nc, nc$var$SHAPE_LENG)), as.numeric(dataFrame$SHAPE_LENG))
	expect_equal(ncatt_get(nc, nc$var$LENGTHKM, "units")$value, "km")
	expect_equal(ncatt_get(nc, nc$var$SHAPE_LENG, "units")$value, "unknown")
  returnDataFrame <- read_instance_data(nc, "instance")
  i <- sapply(dataFrame, is, class2 = "Date")
  dataFrame[i] <- lapply(dataFrame[i], as.character)
  expect_equal(names(dataFrame), names(returnDataFrame)[2:10])
  for(name in names(dataFrame)) {
  	expect_equal(class(dataFrame[name][[1]]), class(returnDataFrame[name][[1]]))
  }
  expect_error(read_instance_data(nc, "garbage"), "The instance dimension was not found in the provided NetCDF object.")
  })

test_that("instance data can be added to an existing netcdf file.", {
	# hucPolygons <- readOGR(dsn = system.file('extdata','example_huc_eta.json', package = 'netcdf.dsg'),"OGRGeoJSON", stringsAsFactors = FALSE)
	hucPolygons <- readRDS("data/hucPolygonData.rds")
	outFile <- tempfile()
	c <- file.copy(system.file('extdata','hucDemo/example_huc_eta.nc', package = 'netcdf.dsg'), outFile)
	nc_file <- write_instance_data(outFile, hucPolygons@data, "station")
	nc <- nc_open(nc_file)
	expect_true(all(names(hucPolygons@data) %in% names(nc$var)))
	orig_names <- c("lat", "lon", "station_name", "et")
	expect_true(all(orig_names %in% names(nc$var)))
}) 