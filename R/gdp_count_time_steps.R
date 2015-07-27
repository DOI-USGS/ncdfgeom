#'@title Get Geo Data Portal time step counter.
#'
#'@description
#'A utility function to help figure out how big a GDP file is and get the time steps in it.
#'
#'@param gdp_file A Geo Data Portal output file containing one statistic, currently no support for multiple statistic output files.
#'@param originDate The origin date desired for the time steps to be returned. This is passed to julian as the origin input.
#'@param expected_times A rough estimate of how many time steps to expect in the file. It is doubled to preallocate an array so no need to be precise, just order of magnitude.
#'
#'@return times A vector of time steps as returned by the Julian function suitable to be passed into a NetCDF file.
#'
#'@export
gdp_count_time_steps<-function(GDPFile,originDate,expected_times) 
{
	times<-1:(expected_times*2)
	time_step<-1
	row<-1
	con<-file(GDPFile, open='r')
	while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0)
	{
		if (row<4)
		{
			row<-row+1
		}	else {
			line_vec<-strsplit(line,split=",")[[1]]
			times[time_step]<-round(julian(strptime(line_vec[1],"%Y-%m-%dT%H:%M:%SZ"),origin=originDate)[[1]])
			time_step<-time_step+1
			row<-row+1
		}
	}
	close(con)
	time_steps<-time_step-1
	times<-times[0:time_steps]
	return(times)
}