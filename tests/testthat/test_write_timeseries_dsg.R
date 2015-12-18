context("Create Station Timeseries DSG File")
library(geoknife)
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
	nc_file<-'test_output.nc'
	
	# Could use this code to generate a non-geoknife required rda file.
	gdp_file<-'data/yahara_alb_gdp_file.csv'
	attribute_file<-'data/yahara_alb_attributes.csv'
	
	# This is a csv dumped out of the yahara_alb dbf file.
	attributes<-read.csv(attribute_file,colClasses='character')
	
	lats<-attributes$YCOORD
	lons<-attributes$XCOORD
	alts<-rep(1,length(lats))
	
	# Using Geoknife to get at the timeseries.
	data<-parseTimeseries(gdp_file,delim=',',with.units=TRUE)
	data_frame<-data[2:(ncol(data)-3)]
	time<-data$DateTime
	long_name=paste(data$variable[1], 'area weighted', data$statistic[1], 'in', data$units[1], sep=' ')
	meta<-list(name=data$variable[1],long_name=long_name)
	
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
