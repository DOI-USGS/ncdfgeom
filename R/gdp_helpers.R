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

GDPIDsAndUnits<-function(GDPFile)
{
	con<-file(GDPFile, open='r')
	for (i in 1:3)
	{
		line <- readLines(con, n = 1)
		if (substr(line,1,1)=="#")
		{
			variable<-strsplit(line,split=' ')[[1]][2]
		}
		else if (substr(line,1,1)==",")
		{
			# This will need a 'unique' call if multiple statistics are in the GDP output file.
			ids<-strsplit(line,split=',')[[1]] 
			ids<-ids[2:length(ids)]
			nc_stations <- ids
			for (i in 1:length(nc_stations))
				if (nc_stations[i]=="")
					nc_stations[i]="000000000000"
			print('null station found')
		}
		else if (substr(line,1,8)=="TIMESTEP")
		{
			print('Parsing stats and units.')
			stats<-strsplit(line,split=',')[[1]]
			stats<-stats[2:length(stats)]
			units<-stats
			for(i in 1:length(stats))
			{
				units[i]<-strsplit(stats[i],split='[()]')[[1]][2]
				stats[i]<-strsplit(stats[i],split='[()]')[[1]][1]
			}
			unique_stats<-unique(stats)
			unique_units<-unique_stats
			# This is stupid but it'll work.
			for (i in 1:length(unique_stats))
			{
				for (j in 1:length(stats))
				{
					if (stats[j]==unique_stats[i]) 
					{
						unique_units[i]<-units[j]
					}
				}
			}
			var_lookup<-array(length(unique_stats),length(stats))
			nc_data_var_unique_name<-unique_stats
			for (i in 1:length(unique_stats))
			{ 
				nc_data_var_unique_name[i]<-paste(unique_stats[i],"_",variable,sep="")
			}
		}
	}
	close(con)
	return(list(nc_stations=nc_stations,nc_data_var_unique_name=nc_data_var_unique_name,unique_units=unique_units))
}