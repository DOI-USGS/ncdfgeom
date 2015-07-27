countTimeSteps<-function(GDPFile,originDate,expected_times) 
{
	times<-1:expected_times*2
	time_step<-1
	row<-1
	file_rows<-as.numeric(strsplit(system(paste("wc -l ",GDPFile,sep=""),intern=TRUE),split=" ")[[1]][6])
	print(file_rows)
	con<-file(GDPFile, open='r')
	while (row<=file_rows)
	{
		if (row<4)
		{
			line <- readLines(con, n = 1)
			row<-row+1
		}
		else
		{
			line <- readLines(con, n = 1)
			line_vec<-strsplit(line,split=",")[[1]]
			times[time_step]<-round(julian(strptime(line_vec[1],"%Y-%m-%dT%H:%M:%SZ"),origin=originDate)[[1]])
			time_step<-time_step+1
			print(line_vec[1])
			print(times[time_step-1])
			row<-row+1
		}
	}
	close(con)
	time_steps<-time_step-1
	times<-times[0:time_steps]
	return(times)
}