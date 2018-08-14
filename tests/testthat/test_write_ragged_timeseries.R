
context("ragged netcdf timeseries")

test_that("characterization testing for write_ragged_timeseries", { 
	
	ok <- readRDS("data/soilmoisturetools/ok.rds")
	ok_meta <- readRDS("data/soilmoisturetools/ok_meta_unique.rds")[, c("station", "latitude", "longitude", "elevation")]
	
	ok <- dplyr::rename(dplyr::left_join(ok, ok_meta, by = "station"),
											"station_name" = "station", "time" = "datetime", 
											"soil_moisture" = "value", "soil_moisture_depth" = "depth_cm", 
											"lat" = "latitude", "lon" = "longitude", "alt" = "elevation")
	
	test_file <- tempfile()
	write_ragged_timeseries_dsg(nc_file = test_file, 
															all_data = ok, 
															data_units = list(soil_moisture='%', 
																								soil_moisture_depth='inches'))
	
	nc <- nc_open(test_file)
	
	expect_equal(nc$dim$station$len, length(unique(ok$station_name)))
	expect_equal(nc$dim$obs$len, nrow(ok))
	
	test_station <- ok_meta$station[1]
	test_station_data <- dplyr::filter(ok, station_name == test_station)
	
	nc_stations <- ncvar_get(nc, nc$var$station_name)
	test_station_index <- which(nc_stations == test_station)
	
	nc_stride <- ncvar_get(nc, nc$var$row_size)
	nc_stop <- cumsum(nc_stride)
	nc_start <- nc_stop - nc_stride + 1
	
	expect_equal(max(nc_stop), nc$dim$obs$len)
	
	expect_equal(as.numeric(ncvar_get(nc, nc$var$soil_moisture, 
																		start = nc_start[test_station_index], 
																		count = nc_stride[test_station_index])), 
							 test_station_data$soil_moisture)
})