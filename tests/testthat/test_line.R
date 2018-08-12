library(ncdf4)
library(sp)
library(sf)

context("NCDF SG line tests")

test_that("linedata works", {
  lineData <- readRDS("data/lineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc, "x")), st_coordinates(lineData)[,"X"])
  expect_equal(as.numeric(ncvar_get(nc, "y")), st_coordinates(lineData)[,"Y"])

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "line")

  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})

test_that("multiLine data works", {
  lineData <- readRDS("data/multiLineData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  num_coords <- c(nrow(st_geometry(lineData)[[1]][[1]]),
  								nrow(st_geometry(lineData)[[1]][[2]]))
  
  expect_equal(as.numeric(ncvar_get(nc,pkg.env$node_count_var_name)),
               sum(num_coords))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$part_node_count_var_name)),
               num_coords)

  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})

test_that("multiline data frame works", {
  lineData <- readRDS("data/multiLineData.rds")
  testdata<-as.data.frame(list("name"=c("test_name"), "id"=c(1)), stringsAsFactors = FALSE)
  lineData <- dplyr::bind_cols(lineData, testdata)
  instance_names <- lineData$name
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = lineData, instance_names = instance_names)

  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")

  returnLineData<-FromNCDFSG(nc_file)
  compareSL(lineData, returnLineData)
})
