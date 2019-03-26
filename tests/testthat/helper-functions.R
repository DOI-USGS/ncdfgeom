library(RNetCDF)
library(ncdf4)
library(sf)
library(sp)
compareSP <- function(polygonData, returnPolyData) {
	polygonData <- check_geomData(polygonData)
	returnPolyData <- check_geomData(returnPolyData)	
  expect_equal(length(polygonData@polygons[[1]]@Polygons), length(returnPolyData@polygons[[1]]@Polygons))
  for(i in 1:length(length(polygonData@polygons[[1]]@Polygons))) {
    expect_equal(as.numeric(returnPolyData@polygons[[1]]@Polygons[[i]]@coords),
                 as.numeric(polygonData@polygons[[1]]@Polygons[[i]]@coords))
    expect_equal(as.numeric(returnPolyData@polygons[[1]]@Polygons[[i]]@ringDir),
                 as.numeric(polygonData@polygons[[1]]@Polygons[[i]]@ringDir))
    # expect_equal(polygonData@polygons[[1]]@Polygons[[i]], returnPolyData@polygons[[1]]@Polygons[[i]]) # checks attributes, not sure it's work testing them.
  }
  expect_equal(polygonData@polygons[[1]]@area, returnPolyData@polygons[[1]]@area)
  # expect_equal(polygonData@polygons[[1]]@plotOrder, returnPolyData@polygons[[1]]@plotOrder) # Don't want to worry about plot order right now.
  expect_equal(polygonData@polygons[[1]]@labpt, returnPolyData@polygons[[1]]@labpt)
  # expect_equal(polygonData@polygons[[1]]@ID, returnPolyData@polygons[[1]]@ID) # maptools 0 indexes others 1 index. Not roundtripping this yet.
}

compareSL <- function(lineData, returnLineData) {
	lineData <- check_geomData(lineData)
	returnLineData <- check_geomData(returnLineData)	
  expect_equal(length(lineData@lines[[1]]@Lines), length(returnLineData@lines[[1]]@Lines))
  for(i in 1:length(length(lineData@lines[[1]]@Lines))) {
    expect_equal(as.numeric(returnLineData@lines[[1]]@Lines[[i]]@coords),
                 as.numeric(lineData@lines[[1]]@Lines[[i]]@coords))
    # expect_equal(lineData@lines[[1]]@Lines[[i]], returnLineData@lines[[1]]@Lines[[i]]) # checks attributes, not sure it's work testing them.
  }
  # expect_equal(lineData@lines[[1]]@ID, returnLineData@lines[[1]]@ID) # maptools 0 indexes others 1 index. Not roundtripping this yet.
}

checkAllPoly <- function(polygonData, node_count, part_node_count = NULL, part_type = NULL) {
	polygonData <- check_geomData(polygonData)
  i<-1 # counter for parts
  for(g in 1:length(polygonData@polygons)) {
    j<-0 # counter for coords in a geom
    for(p in 1:length(polygonData@polygons[[g]]@Polygons)) {
        if(polygonData@polygons[[g]]@Polygons[[p]]@hole) {
          expect_equal(part_type[i], pkg.env$hole_val)
        } else {expect_equal(part_type[i], pkg.env$multi_val) }
      pCount <- length(polygonData@polygons[[g]]@Polygons[[p]]@coords[,1])
      expect_equal(part_node_count[i],
                   pCount)
      i <- i + 1
      j <- j + pCount
    }
    expect_equal(node_count[g],j)
  }
}

get_fixture_data <- function(geom_type) {
  fixtureData <- jsonlite::fromJSON("data/fixture_wkt.json")
  
  return(sf::st_sf(geom = sf::st_as_sfc(fixtureData[["2d"]][geom_type]), 
                   crs = "+init=epsg:4326"))
}

get_sample_timeseries_data <- function() {
  
  yahara <- sf::read_sf("data/Yahara_alb/Yahara_River_HRUs_alb_eq.shp")
  lon_lat <- yahara %>%
    sf::st_set_agr("constant") %>%
    sf::st_centroid() %>%
    sf::st_transform(4326) %>%
    sf::st_coordinates()
  
  lats<-lon_lat[,"Y"]
  lons<-lon_lat[,"X"]
  alts<-rep(1,length(lats))
  
  all_data <- geoknife::parseTimeseries("data/yahara_alb_gdp_file.csv",
                                        delim=',',with.units=TRUE)
  var_data <- all_data[2:(ncol(all_data)-3)]
  
  units <- all_data$units[1]
  
  time <- all_data$DateTime
  
  long_name=paste(all_data$variable[1], 'area weighted', 
                  all_data$statistic[1], 'in', 
                  all_data$units[1], sep=' ')
  
  meta<-list(name=all_data$variable[1],
             long_name=long_name)
  
  return(list(all_data = all_data,
              var_data = var_data,
              time = time,
              long_name = long_name,
              meta = meta,
              lons = lons,
              lats = lats,
              alts = alts,
              units = units))
}
