library(ncdf4)

context("NCDF SG Base Fixture Tests")

# Data Prep - this could be included in tests, but then rgeos would be required for tests.
library(jsonlite)
library(sf)
fixtureData<-fromJSON(readLines(system.file('extdata','fixture_wkt.json', package = 'netcdf.dsg'),warn = FALSE))
p <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
multipointData <- st_sf(geom = st_as_sfc(fixtureData[["2d"]]$multipoint), crs = p)
pointData <- st_sf(geom = st_as_sfc(fixtureData[["2d"]]$point), crs = p)
multiPointsData <- st_sf(geom = st_as_sfc(paste0("GEOMETRYCOLLECTION(",fixtureData[["2d"]]$multipoint,", ",fixtureData[["2d"]]$point,")"), id = c("1","2")), crs = p)
lineData <- st_sf(geom = st_as_sfc(fixtureData[["2d"]]$linestring), crs = p)
multiLineData <- st_sf(geom = st_as_sfc(fixtureData[["2d"]]$multilinestring), crs = p)
polygonData <- st_sf(geom = st_as_sfc(fixtureData[["2d"]]$polygon), crs = p)
polygon_holeData <- st_sf(geom = st_as_sfc(fixtureData[["2d"]]$polygon_hole), crs = p)
multipolygonData <- st_sf(geom = st_as_sfc(fixtureData[["2d"]]$multipolygon), crs = p)
multipolygon_holeData <- st_sf(geom = st_as_sfc(fixtureData[["2d"]]$multipolygon_hole), crs = p)
multipolygons_holesData <- st_sf(geom = st_as_sfc(fixtureData[["2d"]]$multipolygons_holes), crs = p)
multigeometries_polygons_holesData <- st_sf(geom = st_as_sfc(paste0("GEOMETRYCOLLECTION(",fixtureData[["2d"]]$multipolygons_holes,", ",fixtureData[["2d"]]$multipolygon_hole,")"), id = c("1","2")), crs = p)
multigeometries_polygons_holesData <- st_sf(geom = st_as_sfc(paste0("GEOMETRYCOLLECTION(",fixtureData[["2d"]]$multipolygons_holes,", ",fixtureData[["2d"]]$multipolygon_hole,")"), id = c("1","2")), crs = p)


saveRDS(multipointData,file="data/multiPointData.rds")
saveRDS(pointData,file="data/pointData.rds")
saveRDS(multiPointsData, file = "data/multiPointsData.rds")
saveRDS(lineData, file="data/lineData.rds")
saveRDS(multiLineData, file="data/multiLineData.rds")
saveRDS(polygonData, file="data/polygonData.rds")
saveRDS(polygon_holeData, file="data/polygon_holeData.rds")
saveRDS(multipolygonData, file="data/multipolygonData.rds")
saveRDS(multipolygon_holeData, file="data/multipolygon_holeData.rds")
saveRDS(multipolygons_holesData, file="data/multipolygons_holes.rds")
saveRDS(multigeometries_polygons_holesData, file="data/multigeometries_polygons_holes.rds")

order<-c("point", "linestring", "polygon",
         "multipoint", "multilinestring", "multipolygon",
         "polygon_hole", "multipolygon_hole", "multipolygons_holes")

namesstr<-c("Point (2D)", "LineString (2D)", "Polygon (2D)",
         "MultiPoint (2D)", "MultiLineString (2D)", "MultiPolygon (2D)",
         "Polygon with One Interior Ring (2D)", "MultiPolygon with One Interior Ring (2D)",
         "Multiple MultiPolygons with Interior Rings (2D)")

sink('examples.md')
cat(paste("# Examples - Contiguous Ragged Arrays  \nCreated",Sys.time(),"  \n\n"))

for(geom in 1:length(namesstr)) {
  cat(paste0("## ", namesstr[geom],"  \nWell Known Text (WKT): ```",fixtureData[["2d"]][order[geom]]),"```  \n")
  fileName<-paste0("sample_",order[geom],".nc")
  if(grepl('point',order[geom])) {
    ToNCDFSG(fileName,readWKT(fixtureData[["2d"]][order[geom]]))
  } else {
    ToNCDFSG(fileName,readWKT(fixtureData[["2d"]][order[geom]]))
  }
  cat("Common Data Language (CDL):\n```  \n")
  t<-system(paste0("ncdump ", fileName),intern = TRUE)
  cat(t,sep = "\n")
  cat("  \n```  \n\n")
  system(paste("rm", fileName))
}
sink()
