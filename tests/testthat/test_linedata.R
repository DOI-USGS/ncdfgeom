context("NCDF SG lineData tests")

test_that("shapefile line data works", {
  lineData <- sf::st_zm(sf::read_sf("data/NHDLine/NHDLine.shp"))
  instance_names <- as.character(lineData$COMID)
  nc_file <- ToNCDFSG(nc_file=tempfile(), 
  										geomData = lineData, 
  										instance_names = instance_names)
  nc<-nc_open(nc_file)
  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
  sf::st_geometry(lineData) <- NULL
  sf::st_geometry(returnLineData) <- NULL
  for(name in names(lineData)) {
  	if(class(lineData[name][[1]]) == "Date") lineData[[name]] <- as.character(lineData[[name]])
    expect_equal(class(lineData[name][[1]]), class(returnLineData[name][[1]]))
  }
  for(name in names(lineData)) {
    expect_equal(c(lineData[name]), c(returnLineData[name]))
  }
})
