context("NCDF SG lineData tests")

# data prep.
# library(rgdal)
# lineData<-readOGR(dsn = "data/NHDLine/NHDLine.shp",
#                   layer = "NHDLine", stringsAsFactors = FALSE)
# i <- sapply(lineData@data, is.factor)
# lineData@data[i] <- lapply(lineData@data[i], as.character)
# saveRDS(lineData,file="data/NHDline_data.rds")

test_that("shapefile line data works", {
  lineData <- readRDS("data/NHDline_data.rds")
  instance_names <- as.character(lineData$COMID)
  nc_file <- ToNCDFSG(nc_file=tempfile(), 
  										geomData = lineData, 
  										instance_names = instance_names)
  nc<-nc_open(nc_file)
  returnLineData<-FromNCDFSG(nc_file)
  i <- sapply(returnLineData@data, is, class2 = "Date")
  returnLineData@data[i] <- lapply(returnLineData@data[i], as.character)
  compareSL(lineData, returnLineData)
  returnLineData <- st_as_sf(returnLineData)
  for(name in names(lineData)) {
    expect_equal(class(lineData[name][[1]]), class(returnLineData[name][[1]]))
  }
  st_geometry(lineData) <- NULL
  st_geometry(returnLineData) <- NULL
  for(name in names(lineData)) {
    expect_equal(c(lineData[name]), c(returnLineData[name]))
  }
})
