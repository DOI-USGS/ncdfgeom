context("gdp helpers")

test_that("GDP Parsing Correct Basic",{
	test_file<-gdp_ids_and_units("data/GDP_test_file.csv")
	expect_equivalent(test_file$nc_stations, "Wisconsin")
	expect_equivalent(test_file$nc_data_var_unique_name, "MEAN_ppt")
	expect_equivalent(test_file$unique_units, "mm/month")
})