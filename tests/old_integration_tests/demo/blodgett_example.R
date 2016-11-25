

# Atributes
nc_title<-'Daymet2 HUC12 Summary'
nc_summary<-'This dataset was generated using the USGS Geo Data Portal. A national coverage of the Daymet2 dataset was aerially averaged for every HUC12 in the country. The NHDPlus Version 2 Watershed Boundary Dataset HUC12s were used for this subset'
nc_date_create<-'2014-11-05'
nc_creator_name='David Blodgett'
nc_creator_email='dblodgett@usgs.gov'
nc_project='U.S. Geological Survey National Water Census'
nc_proc_level='Aerially averaged gridded precipitation.'
nc_data_longName <- 'Area Weighted Mean Precipitation' #Should be a list of long names corresponding to statistics in file to be re-written
nc_data_prec <- 'single'
nc_station_dim_name <- 'HUC12'
nc_station_longname <- '12 Digit Hydrologic Unit'
nc_station_longname_longname <- '12 Digit Hydrologic Unit Watershed Name'
nc_station_longname_dim_length<-80
nc_filename<-'HUC12_daymet.nc'

# If the data was pulled in multipls chunks, the code below will find how many time steps are in the output.
library('utils')
library("ncdf")

setwd('./')

wd<-'./'

originDate<-strptime("1970-01-01","%Y-%m-%d")

expected_times<-400

times<-c()

for (year in 1980:2013)
{
	times<-c(times,countTimeSteps(paste(wd,year,'.csv',sep=''),originDate,expected_times))
}
time_vec<-1:length(times)

GDPFilePath<-'1980.csv'

temp<-GDPIDsAndUnits(GDPFilePath)
nc_stations<-temp$nc_stations
nc_data_var_unique_name<-temp$nc_data_var_unique_name
unique_units<-temp$unique_units

nc_station_dim <- dim.def.ncdf(nc_station_dim_name,'',c(1:length(nc_stations)),create_dimvar=FALSE)
nc_station_name_dim <- dim.def.ncdf('station_name','',c(1:max(unlist(lapply(nc_stations,nchar)))),create_dimvar=FALSE)
nc_station_names <- var.def.ncdf('stations','',list(nc_station_name_dim,nc_station_dim),'',longname=nc_station_longname,prec='char')

nc_lat_var<-var.def.ncdf('lat','degree_north',nc_station_dim,-999,longname='Latitude',prec='double')
nc_lon_var<-var.def.ncdf('lon','degree_east',nc_station_dim,-999,longname='Longitude',prec='double')
nc_time_dim <- dim.def.ncdf('time','',time_vec,create_dimvar=FALSE)
nc_time_var <- var.def.ncdf('time','days since 1970-01-01 00:00:00', nc_time_dim, -999, prec='integer')
nc_station_longname_dim <- dim.def.ncdf('station_longname','',1:nc_station_longname_dim_length,create_dimvar=FALSE)
nc_station_longname_var<-var.def.ncdf('station_longnames','',list(nc_station_longname_dim,nc_station_dim),'',longname=nc_station_longname_longname,prec='char')
nc_file<-create.ncdf(nc_filename,list(nc_station_names,nc_station_longname_var,nc_time_var,nc_lat_var,nc_lon_var))

# Global attributes 
att<-att.put.ncdf(nc_file, 0,'Conventions','CF-1.6')
att<-att.put.ncdf(nc_file, 0,'featureType','timeSeries')
att<-att.put.ncdf(nc_file, 0,'cdm_data_type','Station')
att<-att.put.ncdf(nc_file, 0,'standard_name_vocabulary','CF-1.6')
att<-att.put.ncdf(nc_file, 0,'title',nc_title)
att<-att.put.ncdf(nc_file, 0,'summary',nc_summary)
att<-att.put.ncdf(nc_file, 0,'date_created',nc_date_create)
att<-att.put.ncdf(nc_file, 0,'creator_name',nc_creator_name)
att<-att.put.ncdf(nc_file, 0,'creator_email',nc_creator_email)
att<-att.put.ncdf(nc_file, 0,'project',nc_project)
att<-att.put.ncdf(nc_file, 0,'processing_level',nc_proc_level)
# Attributes of coordinate variables 
att<-att.put.ncdf(nc_file,nc_time_var,'standard_name','time');
att<-att.put.ncdf(nc_file,nc_station_names,'standard_name','station_id')
att<-att.put.ncdf(nc_file,nc_station_names,'cf_role','timeseries_id')
# Attributes of station attribute variables 
att<-att.put.ncdf(nc_file,nc_lat_var,'standard_name','latitude')
att<-att.put.ncdf(nc_file,nc_lon_var,'standard_name','longitude')


# For HUC12s, this finds the HUC12 name and the lat/lon.
attributes<-read.csv('attributes.txt',colClasses='character')
nc_station_longnames<-1:length(nc_stations)
nc_lats<-1:length(nc_stations)
nc_lons<-1:length(nc_stations)
for (i in 1:length(nc_stations))
{
	ind<-min(which(attributes$HUC_12==nc_stations[i],arr.ind=TRUE))
	nc_station_longnames[i] <- ""
	nc_lats[i] <- NA
	nc_lons[i] <- NA
	try(nc_station_longnames[i] <- attributes$HU_12_NAME[ind],silent=TRUE)
	try(nc_lats[i] <- attributes$lat[ind],silent=TRUE)
	try(nc_lons[i] <- attributes$lon[ind],silent=TRUE)
}

# Write to NetCDF File.
var<-put.var.ncdf(nc_file,nc_station_names,nc_stations)
var<-put.var.ncdf(nc_file,nc_station_longname_var,attributes$HU_12_NAME)
var<-put.var.ncdf(nc_file,nc_time_var,times)
var<-put.var.ncdf(nc_file,nc_lat_var,nc_lats)
var<-put.var.ncdf(nc_file,nc_lon_var,nc_lons)

# Put the unique variables into the netCDF file.
for (i in 1:length(nc_data_var_unique_name))
{
	nc_values<-var.def.ncdf(nc_data_var_unique_name[i],unique_units[i],list(nc_time_dim,nc_station_dim),-999,longname=nc_data_longName[i], prec=nc_data_prec)
	varr<-var.add.ncdf(nc_file,nc_values)
	close.ncdf(nc_file)
	nc_file<-open.ncdf(nc_filename,write=TRUE)
	att<-att.put.ncdf(nc_file,nc_data_var_unique_name[i],'coordinates','time lat lon')
}

close.ncdf(nc_file)

nc_file<-open.ncdf(nc_filename,write=TRUE)
data<-array(dim=c(length(times),83016))
j<-1
time_step<-1
for (year in 1980:2013)
{
	file_rows<-as.numeric(strsplit(system(paste("wc -l ",year,".csv",sep=""),intern=TRUE),split=" ")[[1]][6])
	i<-1
	con<-file(paste(year,".csv",sep=""), open='r')
	while (i<=file_rows)
	{
		if (i<4)
		{
			line <- readLines(con, n = 1)
			print('Header Line')
			i<-i+1
		}
		else
		{
			line <- readLines(con, n = 1)
			line_vec<-strsplit(line,split=",")[[1]]
			times[time_step]<-round(julian(strptime(line_vec[1],"%Y-%m-%dT%H:%M:%SZ"),origin=strptime("1980-01-01","%Y-%m-%d")))[[1]]
			time_step<-time_step+1
			data[j,]<-as.numeric(line_vec[2:length(line_vec)])
			print(line_vec[1])
			i<-i+1
			j<-j+1
			if (i>file_rows) break
		}
	}
	close(con)
}
print('Putting data into NetCDF File.')

# Round to the nearest integer for precip and et.
data<-round(data)
varr<-put.var.ncdf(nc_file,nc_data_var_unique_name,data)
close.ncdf(nc_file)