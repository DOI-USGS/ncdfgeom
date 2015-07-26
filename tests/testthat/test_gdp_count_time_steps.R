context("gdp helpers count time steps")

test_that("GDP Count Time Steps",{
	times<-gdp_count_time_steps("data/gdp_basic_test_file.csv",originDate=strptime("1900-01-01","%Y-%m-%d"),expected_times=2)
	expect_equivalent(times, c(0,31,59))
})