context("NCDF SG pointData tests")

# data prep.
# library(rgdal)
# shapeData<-readOGR(dsn = "data/se_sites/se_sitest.shp",
#                    layer = "se_sitest",
#                    stringsAsFactors = FALSE)
# i <- sapply(shapeData@data, is.factor)
# shapeData@data[i] <- lapply(shapeData@data[i], as.character)
# saveRDS(shapeData,file="data/se_points_data.rds")

test_that("shapefile_point", {
  pointData <- readRDS("data/se_points_data.rds")
  nc_file<-ToNCDFSG(nc_file = tempfile(), geomData = pointData, instance_names = pointData@data$station_nm)
  nc<-nc_open(nc_file)
  expect_true(all(names(pointData@data) %in% names(nc$var)))
  expect_equal(as.character(pointData@data$station_nm),as.character(ncvar_get(nc, nc$var$station_nm)))
  expect_equal(length(ncvar_get(nc, nc$var$y)), length(pointData@coords[,2]))
  expect_equal(length(ncvar_get(nc, nc$var$x)), length(pointData@coords[,1]))
  expect_equal(sum(ncvar_get(nc, nc$var$y)), sum(pointData@coords[,2]))
  expect_equal(sum(ncvar_get(nc, nc$var$x)), sum(pointData@coords[,1]))
  expect_equal(as.character(ncvar_get(nc, nc$var$site_no)), pointData@data$site_no)
  expect_equal(as.numeric(ncvar_get(nc, nc$var$drain_area)), pointData@data$drain_area)
  returnPointData<-FromNCDFSG(nc_file)
  expect_equal(as.numeric(pointData@coords), as.numeric(returnPointData@coords))
  expect_equal(as.numeric(pointData@bbox), as.numeric(returnPointData@bbox))
})
