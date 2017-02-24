library("ncdf4")

context("Test creating a netcdf file with instance data content.")

test_that("Point data can be written", {
  dataFrame <- read.csv("data/yahara_alb_attributes.csv")
  nc_file <- write_point_dsg(nc_file=tempfile(), lats = dataFrame$YCOORD, lons = dataFrame$XCOORD, 
  													 alts = rep(0, length(dataFrame$XCOORD)),
  													 feature_names = dataFrame$ID,
  													 data = dataFrame[c("GRIDCODE", "X_COORD", "Y_COORD")],
  													 data_units = c("unitless", "m", "m"))
  nc <- nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")
  expect_equal(as.character(ncvar_get(nc, nc$var$feature_name)), as.character(dataFrame$ID))
  expect_equal(as.character(ncvar_get(nc, nc$var$GRIDCODE)), as.character(dataFrame$GRIDCODE))
  expect_equal(as.numeric(ncvar_get(nc, nc$var$Y_COORD)), as.numeric(dataFrame$Y_COORD))
  expect_equal(ncatt_get(nc,0,"Conventions")$value,"CF-1.7")
  expect_equal(ncatt_get(nc,0,"featureType")$value,"point")
  expect_equal(ncatt_get(nc,0,"cdm_data_type")$value,"Station")
  expect_equal(ncatt_get(nc, nc$var$GRIDCODE, "units")$value, "unitless")
  expect_equal(ncatt_get(nc, nc$var$Y_COORD, "units")$value, "m")
  })

