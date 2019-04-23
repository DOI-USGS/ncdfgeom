context("orthogonal netcdf timeseries")

test_that("Create basic DSG file", {
  unlink(nc_file)
  nc_summary<-'test summary'
  nc_date_create<-'2099-01-01'
  nc_creator_name='test creator'
  nc_creator_email='test@test.com'
  nc_project='testthat ncdfgeom'
  nc_proc_level='just a test no processing'
  nc_title<-'test title'
  global_attributes<-list(title = nc_title, summary = nc_summary, date_created=nc_date_create, 
                          creator_name=nc_creator_name,creator_email=nc_creator_email,
                          project=nc_project, processing_level=nc_proc_level)
  
  test_data <- get_sample_timeseries_data()
  
  testnc<-write_timeseries_dsg(nc_file, 
                               names(test_data$var_data), 
                               test_data$lats, test_data$lons, 
                               as.character(test_data$time), 
                               test_data$var, 
                               test_data$alts, 
                               data_unit=test_data$units,	
                               data_prec='double',
                               data_metadata=test_data$meta,
                               attributes=global_attributes)
  
  expect_error(
    testnc<-write_timeseries_dsg(nc_file, 
                                 names(test_data$var_data), 
                                 test_data$lats, test_data$lons, 
                                 as.character(test_data$time), 
                                 test_data$var, 
                                 test_data$alts[1], 
                                 data_unit=test_data$units,	
                                 data_prec='double',
                                 data_metadata=test_data$meta,
                                 attributes=global_attributes),
    "File already exists and overwrite is false."
  )
  
  expect_error(
  testnc<-write_timeseries_dsg(nc_file, 
                               names(test_data$var_data), 
                               test_data$lats, test_data$lons, 
                               as.character(test_data$time), 
                               test_data$var, 
                               test_data$alts[1], 
                               data_unit=test_data$units,	
                               data_prec='double',
                               data_metadata=test_data$meta,
                               attributes=global_attributes,
                               overwrite = TRUE),
  "instance_names and alts must all be vectors of the same length"
  )
  
  expect_error(
    testnc<-write_timeseries_dsg(nc_file, 
                                 names(test_data$var_data), 
                                 test_data$lats[1], test_data$lons, 
                                 as.character(test_data$time), 
                                 test_data$var, 
                                 test_data$alts, 
                                 data_unit=test_data$units,	
                                 data_prec='double',
                                 data_metadata=test_data$meta,
                                 attributes=global_attributes,
                                 overwrite = TRUE),
    "instance_names, lats, and lons must all be vectors of the same length"
  )
  
  expect_error(
    testnc<-write_timeseries_dsg(nc_file, 
                                 names(test_data$var_data), 
                                 test_data$lats, test_data$lons, 
                                 as.character(test_data$time), 
                                 test_data$var[1:10,], 
                                 test_data$alts, 
                                 data_unit=test_data$units,	
                                 data_prec='double',
                                 data_metadata=test_data$meta,
                                 attributes=global_attributes,
                                 overwrite = TRUE),
    "The length of times must match the number of rows in data"
  )
  
  expect_error(
    testnc<-write_timeseries_dsg(nc_file, 
                                 names(test_data$var_data), 
                                 test_data$lats, test_data$lons, 
                                 as.character(test_data$time), 
                                 test_data$var[, 1:10], 
                                 test_data$alts, 
                                 data_unit=test_data$units,	
                                 data_prec='double',
                                 data_metadata=test_data$meta,
                                 attributes=global_attributes,
                                 overwrite = TRUE),
    "number of data columns must equal the number of stations"
  )
  
  broken <- test_data$var
  broken$`1` <- as.integer(broken$`1`)
  
  expect_error(
    testnc<-write_timeseries_dsg(nc_file, 
                                 names(test_data$var_data), 
                                 test_data$lats, test_data$lons, 
                                 as.character(test_data$time), 
                                 broken, 
                                 test_data$alts, 
                                 data_unit=test_data$units,	
                                 data_prec='double',
                                 data_metadata=test_data$meta,
                                 attributes=global_attributes,
                                 overwrite = TRUE),
    "All the collumns in the input dataframe must be of the same type."
  )
  
  test_data$meta <- list(name = "duplicate", long_name = "test")
  
  testnc<-write_timeseries_dsg(nc_file, 
                               names(test_data$var_data), 
                               test_data$lats, test_data$lons, 
                               test_data$time, test_data$var, 
                               test_data$alts, 
                               data_unit=test_data$units,	
                               data_prec='double',
                               data_metadata=test_data$meta,
                               attributes=global_attributes,
                               add_to_existing = TRUE,
                               overwrite = FALSE)
  
  testnc<-nc_open(nc_file)
  
  expect_equivalent(length(testnc$dim[[pkg.env$instance_dim_name]]$vals),71)
  expect_equivalent(ncatt_get(testnc,varid=0,"Conventions")$value, pkg.env$cf_version)
  expect_equivalent(ncatt_get(testnc,varid=0,"cdm_data_type")$value,"Station")
  expect_equivalent(ncatt_get(testnc,varid=0,"standard_name_vocabulary")$value, pkg.env$cf_version)
  expect_equivalent(ncatt_get(testnc,varid=pkg.env$dsg_timeseries_id, "cf_role")$value, pkg.env$timeseries_id_cf_role)
  expect_equivalent(ncatt_get(testnc,varid=pkg.env$time_var_name, "standard_name")$value, pkg.env$time_var_standard_name)
  expect_equivalent(ncatt_get(testnc,varid=pkg.env$lat_coord_var_name,"standard_name")$value,pkg.env$lat_coord_var_standard_name)
  expect_equivalent(ncatt_get(testnc,varid=pkg.env$lon_coord_var_name,"standard_name")$value,pkg.env$lon_coord_var_standard_name)
  expect_equivalent(ncatt_get(testnc,varid=test_data$all_data$variable[1],'long_name')$value,test_data$long_name)
  expect_equivalent(ncvar_get(testnc,varid=pkg.env$dsg_timeseries_id)[1],"1")
  expect_equivalent(ncvar_get(testnc,varid="BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1")[,1],test_data$all_data$`1`)
  expect_equivalent(ncvar_get(testnc,varid="BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1")[,71],test_data$all_data$`71`)
  expect_equivalent(testnc$var$`BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1`$units,"mm/d")
  expect_equivalent(ncatt_get(testnc,varid=0,"summary")$value,'test summary')
  
  # covers no altitude and iteration to write many rows.
  test_dat2 <- dplyr::bind_rows(test_data$var_data, test_data$var_data)
  time <- c(test_data$time,test_data$time)
  testnc<-write_timeseries_dsg(tempfile(), 
                               names(test_data$var_data), 
                               test_data$lats, test_data$lons, 
                               time, test_dat2,
                               data_unit=test_data$units,	
                               data_prec='double',
                               data_metadata=test_data$meta,
                               attributes=global_attributes)
  
  testnc<-nc_open(testnc)
  expect(testnc$dim$time$len == 1460)
  
  char_test <- dplyr::mutate_all(test_dat2, as.character)
  time <- c(test_data$time,test_data$time)
  testnc<-write_timeseries_dsg(tempfile(), 
                               names(test_data$var_data), 
                               test_data$lats, test_data$lons, 
                               time, char_test,
                               data_unit=test_data$units,	
                               data_prec='char',
                               data_metadata=test_data$meta,
                               attributes=global_attributes)
  
  testnc<-nc_open(testnc)
  expect(testnc$dim$time$len == 1460)
  
  testthat::skip_on_cran()
  expect("duplicate" %in% names(testnc$var), failure_message = names(testnc$var))
  
  nc_close(testnc)
  
  test_data$meta <- list(name = "character", long_name = "test")
  char_test <- dplyr::mutate_all(test_data$var_data, as.character)
  testnc<-write_timeseries_dsg(nc_file, 
                               names(test_data$var_data), 
                               test_data$lats, test_data$lons, 
                               test_data$time, char_test, 
                               test_data$alts, 
                               data_unit=test_data$units,	
                               data_prec='char',
                               data_metadata=test_data$meta,
                               attributes=global_attributes,
                               add_to_existing = TRUE)
  
  testnc<-nc_open(nc_file)
  
  expect("character" %in% names(testnc$var), failure_message = names(testnc$var))
})
  
test_that("bork the file", {
  
	test_data <- get_sample_timeseries_data()
	
	testlist<-read_timeseries_dsg(nc_file)
	
	expect_equivalent(length(testlist$time), length(test_data$time))
	expect_equivalent(as.numeric(testlist$lats), as.numeric(test_data$lats))
	expect_equivalent(as.numeric(testlist$lons), as.numeric(test_data$lons))
	expect_equivalent(as.numeric(testlist$alts), as.numeric(test_data$alts))
	expect_equivalent(testlist$data_unit[1], test_data$units) # could be tricky if there are multiple variables in the netcdf file.
	expect_equivalent(testlist$data_prec[1],'NC_DOUBLE')
	expect_equivalent(testlist$varmeta[[1]]$name,test_data$all_data$variable[1])
	expect_equivalent(testlist$varmeta[[1]]$long_name,test_data$long_name)
	expect_equivalent(testlist$global_attributes$nc_summary,'test summary')
	expect_equivalent(testlist$global_attributes$nc_date_created,'2099-01-01')
	expect_equivalent(testlist$global_attributes$nc_creator_name,'test creator')
	expect_equivalent(testlist$global_attributes$nc_creator_email,'test@test.com')
	expect_equivalent(testlist$global_attributes$nc_project,'testthat ncdfgeom')
	expect_equivalent(testlist$global_attributes$nc_proc_level,'just a test no processing')
	expect_equivalent(testlist$global_attributes$nc_title,'test title')
	expect_equivalent(testlist$data_frames[1][[1]],test_data$var_data) # Plan to have the dataframes work for 1 to many variables.
	
  nc_file_borked <- tempfile()
  file.copy(nc_file, nc_file_borked, overwrite = TRUE)
  nc <- RNetCDF::open.nc(nc_file_borked, write = TRUE)
  att.delete.nc(nc, "BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1", "coordinates")
  att.delete.nc(nc, "duplicate", "coordinates")
  att.delete.nc(nc, "character", "coordinates")
  close.nc(nc)
  expect_warning(
  testlist<-read_timeseries_dsg(nc_file_borked), 
  "no data variables found, attempting to infer via shared dimensions")

  file.copy(nc_file, nc_file_borked, overwrite = TRUE)
  nc <- RNetCDF::open.nc(nc_file_borked, write = TRUE)
  att.delete.nc(nc, "NC_GLOBAL", "Conventions")
  close.nc(nc)
  expect_warning(
    testlist<-read_timeseries_dsg(nc_file_borked), 
    "File does not advertise CF conventions, unexpected behavior may result.")
  
  file.copy(nc_file, nc_file_borked, overwrite = TRUE)
  nc <- RNetCDF::open.nc(nc_file_borked, write = TRUE)
  att.delete.nc(nc, "NC_GLOBAL", "featureType")
  close.nc(nc)
  expect_warning(
    testlist<-read_timeseries_dsg(nc_file_borked), 
    "File does not advertise use of the CF timeseries featureType, unexpected behavior may result.")
  
  file.copy(nc_file, nc_file_borked, overwrite = TRUE)
  nc <- RNetCDF::open.nc(nc_file_borked, write = TRUE)
  att.delete.nc(nc, "instance_name", "cf_role")
  close.nc(nc)
  expect_error(
    testlist<-read_timeseries_dsg(nc_file_borked), 
    "A timeseries id variable was not found in the file.")
  
  file.copy(nc_file, nc_file_borked, overwrite = TRUE)
  nc <- RNetCDF::open.nc(nc_file_borked, write = TRUE)
  att.delete.nc(nc, "BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1", "coordinates")
  att.delete.nc(nc, "duplicate", "coordinates")
  att.delete.nc(nc, "character", "coordinates")
  att.delete.nc(nc, "lat", "standard_name")
  att.delete.nc(nc, "lon", "standard_name")
  att.delete.nc(nc, "time", "standard_name")
  close.nc(nc)
  warn <- capture_warnings(testlist<-read_timeseries_dsg(nc_file_borked))
  expect(all(c("no data variables found, attempting to infer via shared dimensions",
           "no latitude coordinate found",                                     
           "no longitude coordinate found") %in% warn))
    
  nc <- RNetCDF::open.nc(nc_file_borked, write = TRUE)
  att.delete.nc(nc, "time", "units")
  close.nc(nc)
  expect_error(
    testlist<-read_timeseries_dsg(nc_file_borked), 
    "No coordinates declarations were found in the file.")
  
  unlink(nc_file)
  unlink(nc_file_borked)
})

test_that('soilmoisturetools data writes as expected', {
  ok<-readRDS("data/soilmoisturetools/ok.rds")
  ok_meta<-readRDS("data/soilmoisturetools/ok_meta.rds")
  attributes <- list(
    title = 'National Soil Moisture Network SOS',
    abstract = 'This service provides soil moisture data from the U.S.
    National Soil Moisture Network Pilot and serves data from SCAN, CRN,
    West Texas and Oklahoma Mesonets. This SOS web service delivers the data
    using GML.',
    'provider name' = 'U.S. Geological Survey, Office of Water Information,
    Center for Integrated Data Analytics, United States Government',
    'provider site' = 'http://cida.usgs.gov',
    description = 'Percentile of Volumetric Soil Moisture as compared
    to the historical distribution. Percentiles are calculated using
    cumulative distribution functions and range from 0-100.'
  )
  nc_file <- write_timeseries_dsg(
    nc_file = tempfile(),
    instance_names = ok$station,
    lats = ok_meta$latitude,
    lons = ok_meta$longitude,
    alts = ok_meta$elevation,
    times = ok$datetime[1],
    data = as.data.frame(array(ok$value, dim = c(
      1, length(ok$value)
    ))),
    data_unit = "percent",
    data_prec = "double",
    attributes = attributes
  )
  
  nc <- nc_open(nc_file)
  
  expect(file.exists(nc_file))
  unlink(nc_file)
})
