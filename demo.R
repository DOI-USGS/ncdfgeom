library(sp)
library(leaflet)

setwd("~/temp/demo")

file.copy(system.file('extdata','example_huc_eta.nc', package = 'netcdf.dsg'), 
					"hucPolygons.nc", 
					overwrite = TRUE) -> quiet

hucTimeseries <- ncdf4::nc_open("hucPolygons.nc")

et <- ncdf4::ncvar_get(hucTimeseries, hucTimeseries$var$et)
time <- as.POSIXct(hucTimeseries$dim$time$vals * 24*60*60, 
									 origin = strptime("1970-01-01", "%Y-%m-%d"))

plot(time, et[,1], main = "Monthly Evapotranspiration",pch=16)
lines(time, et[,1], pch=16)

hucPolygons <- rgdal::readOGR(system.file('extdata','example_huc_eta.json', 
																					package = 'netcdf.dsg'),
															"OGRGeoJSON", 
															stringsAsFactors = FALSE, verbose = FALSE)

leaflet(hucPolygons) %>% addTiles() %>% addPolygons()

hucPolygons_nc <- netcdf.dsg::ToNCDFSG(nc_file="hucPolygons.nc", 
																			 geomData = hucPolygons, 
																			 instance_names = hucPolygons$HUC12, 
																			 instance_dim_name = "station", 
																			 variables = hucTimeseries$var)

hucPolygons_sp <- netcdf.dsg::FromNCDFSG("hucPolygons.nc")

hucPolygons_shp <- rgdal::writeOGR(hucPolygons_sp, 
																	 "hucPolygons.shp", 
																	 layer = "hucPolygons", 
																	 driver = "ESRI Shapefile")

