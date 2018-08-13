## ----attributes----------------------------------------------------------
attribute_file<-system.file('extdata/yahara_alb_attributes.csv', package = "netcdf.dsg")

# Grab some attributes
attributes <- read.csv(attribute_file,colClasses='character')
lats <- attributes$YCOORD
lons <- attributes$XCOORD
alts <- rep(1,length(lats)) # Making up altitude for the sake of demonstration.

## ----timeseries----------------------------------------------------------
timeseries_file<-system.file('extdata/yahara_alb_gdp_file.csv', package = "netcdf.dsg")
raw_data <- geoknife::parseTimeseries(timeseries_file,delim=',',with.units=TRUE)
timeseries_data <- raw_data[2:(ncol(raw_data)-3)]
time <- raw_data$DateTime
long_name <- paste(raw_data$variable[1], 'area weighted', raw_data$statistic[1], 'in', raw_data$units[1], sep=' ')
meta <- list(name=raw_data$variable[1],long_name=long_name)

## ----write---------------------------------------------------------------
nc_summary<-'example summary'
nc_date_create<-'2099-01-01'
nc_creator_name='example creator'
nc_creator_email='example@test.com'
nc_project='example netcdf.dsg'
nc_proc_level='just an example no processing'
nc_title<-'example title'
global_attributes<-list(title = nc_title, summary = nc_summary, date_created=nc_date_create,
												creator_name=nc_creator_name,creator_email=nc_creator_email,
												project=nc_project, processing_level=nc_proc_level)

nc_file <- netcdf.dsg::write_timeseries_dsg(nc_file = "demo_nc.nc", 
																						station_names = names(timeseries_data),
																						lats = lats, lons = lons, times = time, alts = alts,
																						data = timeseries_data,
																						data_unit = raw_data$units[1],
																						data_prec = 'double',
																						data_metadata = meta,
																						attributes = global_attributes)

nc <- ncdf4::nc_open(nc_file)

## ----dim-----------------------------------------------------------------
names(nc$dim)

## ----var-----------------------------------------------------------------
names(nc$var)

## ----dim2----------------------------------------------------------------
nc$dim$time$len
nc$dim$station$len

## ----read----------------------------------------------------------------
timeseries_dataset <- netcdf.dsg::read_timeseries_dsg(nc_file)
names(timeseries_dataset)

## ----names---------------------------------------------------------------
varNames <- names(timeseries_dataset$varmeta)
timeseries_dataset$data_unit[varNames[1]][[1]]

## ----delete, echo=F, message=F-------------------------------------------
t <- file.remove(nc_file)

