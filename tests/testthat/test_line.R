library(ncdf4)
library(sp)

context("NCDF SG line tests")

test_that("linedata works", {
  lineData <- readRDS("data/lineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc, "x")), as.numeric(lineData@lines[[1]]@Lines[[1]]@coords[,1]))
  expect_equal(as.numeric(ncvar_get(nc, "y")), as.numeric(lineData@lines[[1]]@Lines[[1]]@coords[,2]))

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "line")

  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})

test_that("multiLine data works", {
  lineData <- readRDS("data/multiLineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$node_count_var_name)),
               (length(lineData@lines[[1]]@Lines[[1]]@coords[,1]) +
                length(lineData@lines[[1]]@Lines[[2]]@coords[,1])))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$part_node_count_var_name)),
               c(length(lineData@lines[[1]]@Lines[[1]]@coords[,1]),
                  length(lineData@lines[[1]]@Lines[[2]]@coords[,1])))

  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})

test_that("multiline data frame works", {
  lineData <- readRDS("data/multiLineData.rds")
  testdata<-as.data.frame(list("name"=c("test_name"), "id"=c(1)))
  lineData <- SpatialLinesDataFrame(lineData, testdata)
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData, instance_names = as.character(lineData@data$name))

  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")

  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})
