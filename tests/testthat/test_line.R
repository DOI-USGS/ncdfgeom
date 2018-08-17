context("line")

test_that("linedata works", {
  lineData <- get_fixture_data("linestring")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc, pkg.env$x_nodes)), st_coordinates(lineData)[,"X"])
  expect_equal(as.numeric(ncvar_get(nc, pkg.env$y_nodes)), st_coordinates(lineData)[,"Y"])

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "line")

  returnLineData<-read_geometry(nc_file)
  compareSL(lineData, returnLineData)
})

test_that("multiLine data works", {
  lineData <- get_fixture_data("multilinestring")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = lineData)
  nc<-nc_open(nc_file)

  num_coords <- c(nrow(st_geometry(lineData)[[1]][[1]]),
  								nrow(st_geometry(lineData)[[1]][[2]]))
  
  expect_equal(as.numeric(ncvar_get(nc,pkg.env$node_count_var_name)),
               sum(num_coords))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$part_node_count_var_name)),
               num_coords)

  returnLineData<-read_geometry(nc_file)
  compareSL(lineData, returnLineData)
})

test_that("multiline data frame works", {
  lineData <- get_fixture_data("multilinestring")
  testdata<-as.data.frame(list("name"=c("test_name"), "id"=c(1)), stringsAsFactors = FALSE)
  lineData <- dplyr::bind_cols(lineData, testdata)
  instance_names <- lineData$name
  nc_file <- write_geometry(nc_file=tempfile(), geomData = lineData, instance_names = instance_names)

  nc<-nc_open(nc_file)
  expect_equal(class(nc),"ncdf4")

  returnLineData<-read_geometry(nc_file)
  compareSL(lineData, returnLineData)
})

test_that("shapefile line data works", {
  lineData <- sf::st_zm(sf::read_sf("data/NHDLine/NHDLine.shp"))
  instance_names <- as.character(lineData$COMID)
  nc_file <- write_geometry(nc_file=tempfile(), 
                      geomData = lineData, 
                      instance_names = instance_names)
  nc<-nc_open(nc_file)
  returnLineData<-read_geometry(nc_file)
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
