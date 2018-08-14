library(soilmoisturetools)
library(dplyr)
library(ncdfgeom)
library(reshape2)

ok = ok_data() %>% filter(depth_in < 2)

ok_meta = ok_sites_metadata(ok$station)

#tx = ok_data() %>% filter(depth_in < 2)

#tx_meta = ok_sites_metadata(ok$station)

#scan = scan_data(scan_sites_by_element_code('SMS'), depth = -2, start=Sys.Date()-1, end=Sys.Date())

wide_sites = dcast(ok, datetime~station, mean)
times = wide_sites$datetime
wide_sites = wide_sites[,-1]
station_names = sub(':', '',sub(':', '', names(wide_sites)))
names(wide_sites) = rep('moisture_percentile', ncol(wide_sites))

write_timeseries_dsf('~/ok_more_metadata.nc',station_names, ok_meta$nlat, ok_meta$elon,
								times, wide_sites, data_unit='percent', data_prec ='double', 
								attributes=list(
									title='National Soil Moisture Network SOS', 
									abstract="This service provides soil moisture data from the U.S. National Soil Moisture Network Pilot and serves data from SCAN, CRN, West Texas and Oklahoma Mesonets. This SOS web service delivers the data using OGC's GML.", 
									'provider name'='U.S. Geological Survey, Office of Water Information, Center for Integrated Data Analytics, United States Government', 
									'provider site'='http://cida.usgs.gov', 
									description='Percentile of Volumetric Soil Moisture as compared to the historical distribution. Percentiles are calculated using cumulative distribution functions and range from 0-100.'))

#get.var.ncdf(open.ncdf('~/ok_test.nc'), 'station_name')
