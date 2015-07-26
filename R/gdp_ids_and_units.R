#'@title Get Geo Data Portal station ids and units 
#'
#'@description
#'Parses the first three rows of a Geo Data Portal output file and returns a named list ready for use in creation of netCDF DSG files.
#'
#'@param gdp_file A Geo Data Portal output file containing one statistic, currently no support for multiple statistic output files.
#'
#'@return A named list as follows:\cr
#'nc_stations contains unique station ids,\cr
#'nc_data_var_unique_name contains the variables in the file,\cr
#'unique_units contains the units associated with each variable.
#'
#'@export
gdp_ids_and_units<-function(GDPFile)
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