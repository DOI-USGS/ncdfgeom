context("Create Station Timeseries DSG File")
library(ncdf4)

test_that("Create basic DSG file",{
	nc_summary<-'test summary'
	nc_date_create<-'2099-01-01'
	nc_creator_name='test creator'
	nc_creator_email='test@test.com'
	nc_project='testthat netcdf.dsg'
	nc_proc_level='just a test no processing'
	nc_title<-'test title'
	global_attributes<-list(title = nc_title, summary = nc_summary, date_created=nc_date_create, 
		creator_name=nc_creator_name,creator_email=nc_creator_email,
		project=nc_project, processing_level=nc_proc_level)
	
	nc_file<-'data/test_output.nc'
	
	# See test_read_timeseries_dsg.R for how this data was created.
	load("data/yahara_test_data.rda")
	
	testnc<-write_timeseries_dsg(nc_file, names(data_frame), lats, lons, time, data_frame, 
															 alts, data_unit=data$units[1],	data_prec='double',data_metadata=meta,
															 attributes=global_attributes)
	testnc<-nc_open(nc_file)
	
	expect_equivalent(length(testnc$dim$station$vals),71)
	expect_equivalent(ncatt_get(testnc,varid=0,"Conventions")$value,"CF-1.7")
	expect_equivalent(ncatt_get(testnc,varid=0,"cdm_data_type")$value,"Station")
	expect_equivalent(ncatt_get(testnc,varid=0,"standard_name_vocabulary")$value,"CF-1.7")
	expect_equivalent(ncatt_get(testnc,varid="station_name","standard_name")$value,"station_id")
	expect_equivalent(ncatt_get(testnc,varid="station_name","cf_role")$value,"timeseries_id")
	expect_equivalent(ncatt_get(testnc,varid="time","standard_name")$value,"time")
	expect_equivalent(ncatt_get(testnc,varid="lat","standard_name")$value,"latitude")
	expect_equivalent(ncatt_get(testnc,varid="lon","standard_name")$value,"longitude")
	expect_equivalent(ncatt_get(testnc,varid=data$variable[1],'long_name')$value,long_name)
	expect_equivalent(ncvar_get(testnc,varid="station_name")[1],"1")
	expect_equivalent(ncvar_get(testnc,varid="BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1")[,1],data$`1`)
	expect_equivalent(ncvar_get(testnc,varid="BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1")[,71],data$`71`)
	expect_equivalent(testnc$var$`BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1`$units,"mm/d")
	expect_equivalent(ncatt_get(testnc,varid=0,"summary")$value,'test summary')
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
		station_names = ok$station,
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
