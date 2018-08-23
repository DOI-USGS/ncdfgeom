context("orthoganal netcdf timeseries")

test_that("Create basic DSG file",{
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
  
  nc_file<-'data/test_output.nc'
  
  test_data <- get_sample_timeseries_data()
  
  testnc<-write_timeseries_dsg(nc_file, 
                               names(test_data$var_data), 
                               test_data$lats, test_data$lons, 
                               test_data$time, test_data$var, 
                               test_data$alts, 
                               data_unit=test_data$units,	
                               data_prec='double',
                               data_metadata=test_data$meta,
                               attributes=global_attributes)
  
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
                               add_to_existing = TRUE)
  
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
  expect("duplicate" %in% names(testnc$var))
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
  
  expect_equal(1,1)
})

test_that("Read basic DSG file",{
	
	test_data <- get_sample_timeseries_data()
	
	nc_file<-'data/test_output.nc'
	
	testlist<-read_timeseries_dsg(nc_file)
	
	expect_equivalent(length(testlist$time), length(test_data$time))
	expect_equivalent(as.numeric(testlist$lats), as.numeric(test_data$lats))
	expect_equivalent(as.numeric(testlist$lons), as.numeric(test_data$lons))
	expect_equivalent(as.numeric(testlist$alts), as.numeric(test_data$alts))
	expect_equivalent(testlist$data_unit[1], test_data$units) # could be tricky if there are multiple variables in the netcdf file.
	expect_equivalent(testlist$data_prec[1],'double')
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

	unlink(nc_file)
})
