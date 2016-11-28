library("ncdf4")

context("Test creating a netcdf file with instance data content.")

test_that("A dataframe can be round tripped to netCDF.", {
  dataFrame <- readRDS("data/NHDline_data.rds")@data
  nc_file <- write_instance_data(ncFile=tempfile(), attData = dataFrame)
  nc <- nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(as.character(ncvar_get(nc, nc$var$COMID)), as.character(dataFrame$COMID))
  expect_equal(as.character(ncvar_get(nc, nc$var$FDATE)), as.character(dataFrame$FDATE))
  expect_equal(as.numeric(ncvar_get(nc, nc$var$SHAPE_LENG)), as.numeric(dataFrame$SHAPE_LENG))
  returnDataFrame <- read_instance_data(nc, "instance")
  i <- sapply(dataFrame, is, class2 = "Date")
  dataFrame[i] <- lapply(dataFrame[i], as.character)
  expect_equal(names(dataFrame), names(returnDataFrame)[2:10])
  for(name in names(dataFrame)) {
  	expect_equal(class(dataFrame[name][[1]]), class(returnDataFrame[name][[1]]))
  }
  expect_error(read_instance_data(nc, "garbage"), "The instance dimension was not found in the provided NetCDF object.")
  })

