context("Read Station Timeseries DSG File")

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
	expect_equivalent(testlist$global_attributes$nc_project,'testthat netcdf.dsg')
	expect_equivalent(testlist$global_attributes$nc_proc_level,'just a test no processing')
	expect_equivalent(testlist$global_attributes$nc_title,'test title')
	expect_equivalent(testlist$data_frames[1][[1]],test_data$var_data) # Plan to have the dataframes work for 1 to many variables.
	})