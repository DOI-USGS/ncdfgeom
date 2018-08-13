## ----load, echo=FALSE, message=FALSE, warning=FALSE----------------------
library(sp)
file.copy(system.file('extdata','example_huc_eta.nc', package = 'netcdf.dsg'), 
					"hucPolygons.nc", 
					overwrite = TRUE) -> quiet

## ----libs----------------------------------------------------------------
hucTimeseries <- ncdf4::nc_open("hucPolygons.nc")

hucPolygons <- rgdal::readOGR(system.file('extdata','example_huc_eta.json', 
																					package = 'netcdf.dsg'),
															"OGRGeoJSON", 
															stringsAsFactors = FALSE, verbose = FALSE)
plot(hucPolygons)

## ----demo----------------------------------------------------------------
hucPolygons_nc <- netcdf.dsg::ToNCDFSG(nc_file="hucPolygons.nc", 
																			 geomData = hucPolygons, 
																			 instance_names = hucPolygons$HUC12, 
																			 instance_dim_name = "station", 
																			 variables = hucTimeseries$var)

## ----read, warning=F-----------------------------------------------------
hucPolygons_sp <- netcdf.dsg::FromNCDFSG("hucPolygons.nc")

hucPolygons_shp <- rgdal::writeOGR(hucPolygons_sp, 
																	 "hucPolygons.shp", 
																	 layer = "hucPolygons", 
																	 driver = "ESRI Shapefile")

## ----cleanup, echo=F-----------------------------------------------------
temp <- file.remove("hucPolygons.nc", "hucPolygons.dbf", "hucPolygons.prj", "hucPolygons.shp", "hucPolygons.shx")

