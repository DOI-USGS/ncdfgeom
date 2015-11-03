context("Read Station Timeseries DSG File")
library(geoknife)
test_that("Create basic DSG file",{
	
	nc_file<-'test_output.nc'
	
	testlist<-read_timeseries_dsg(nc_file)
	
	### Could use this code to generate a non-geoknife required rda file.
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
	### Could use this code to generate a non-geoknife required rda file.
	
	expect_equivalent(testlist$time, time)
	expect_equivalent(as.numeric(testlist$lats), as.numeric(lats))
	expect_equivalent(as.numeric(testlist$lons), as.numeric(lons))
	expect_equivalent(as.numeric(testlist$alts), as.numeric(alts))
	expect_equivalent(testlist$data_unit[1], data$units[1]) # could be tricky if there are multiple variables in the netcdf file.
	expect_equivalent(testlist$data_prec[1],'double')
	expect_equivalent(testlist$varmeta[1]$name,data$variable[1])
	expect_equivalent(testlist$varmeta[1]$long_name,long_name)
	expect_equivalent(testlist$data_frames[1],data_frame) # Plan to have the dataframes work for 1 to many variables.
	expect_equivalent(testlist$global_attributes$nc_summary,'test summary')
	expect_equivalent(testlist$global_attributes$nc_date_create,'2099-01-01')
	expect_equivalent(testlist$global_attributes$nc_creator_name,'test creator')
	expect_equivalent(testlist$global_attributes$nc_creator_email,'test@test.com')
	expect_equivalent(testlist$global_attributes$nc_project,'testthat netcdf.dsg')
	expect_equivalent(testlist$global_attributes$nc_proc_level,'just a test no processing')
	expect_equivalent(testlist$global_attributes$nc_title,'test title')
	})