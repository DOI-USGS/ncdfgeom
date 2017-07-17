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
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData, instance_names = as.character(lineData@data$COMID))
  nc<-nc_open(nc_file)
  returnLineData<-FromNCDFSG(nc_file)
  i <- sapply(lineData@data, is, class2 = "Date")
  lineData@data[i] <- lapply(lineData@data[i], as.character)
  compareSL(lineData, returnLineData)
  for(name in names(lineData@data)) {
    expect_equal(class(lineData@data[name][[1]]), class(returnLineData@data[name][[1]]))
  }
  for(name in names(lineData@data)) {
    expect_equal(c(lineData@data[name]), c(returnLineData@data[name]))
  }
})
