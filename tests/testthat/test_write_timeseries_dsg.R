context("Create Station Timeseries DSG File")
library(geoknife)
library(ncdf)
test_that("Create basic DSG file",{
	# # Atributes that need to be passed into NetCDF file.
	# nc_title<-'test'
	# nc_summary<-'test'
	# nc_date_create<-'2099-01-01'
	# nc_creator_name='test R'
	# nc_creator_email='test@R.com'
	# nc_project='test project'
	# nc_proc_level='test processing level'
	# nc_data_longName <- 'test single variable long name' #Should be a list of long names corresponding to statistics in file to be re-written
	# nc_data_prec <- 'single'
	# nc_station_dim_name <- 'gridcode'
	# nc_station_longname <- 'watershed'
	# nc_station_longname_longname <- 'watershed gridcode'
	# nc_station_longname_dim_length<-80
	
	nc_file<-'test_output.nc'
	gdp_file<-'data/yahara_alb_gdp_file.csv'
	attribute_file<-'data/yahara_alb_attributes.csv'
	
	# # Weird that times is required seperate from the data frame.
	# Don't need to do it this way if use geo knife.
	# times<-gdp_count_time_steps(gdp_file,originDate=strptime("1900-01-01","%Y-%m-%d"),expected_times=2)
	
	# This is a csv dumped out of the yahara_alb dbf file.
	attributes<-read.csv(attribute_file,colClasses='character')
	
	lats<-attributes$YCOORD
	lons<-attributes$XCOORD
	alts<-rep(1,length(lats))
	
	# Don't need to do this if use GeoKnife patterns.
	# temp<-gdp_ids_and_units(gdp_file)
	# nc_stations<-temp$nc_stations
	# nc_vars<-temp$nc_data_var_unique_name
	# nc_vars_units<-temp$unique_units
	
	data<-parseTimeseries(gdp_file,delim=',',with.units=TRUE)
	data_frame<-data[2:(ncol(data)-3)]
	time<-data$DateTime
	
	testnc<-write_timeseries_dsg(nc_file, names(data_frame), lats, lons, alts, time, data_frame, data_unit=data$units[1],	data_prec='double')
	testnc<-open.ncdf(nc_file)
	
	expect_equivalent(length(testnc$dim$station$vals),71)
})