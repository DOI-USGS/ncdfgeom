library(ncdf4)

context("NCDF SG polygon tests")

test_that("data for basic polygon", {
  polygonData <- readRDS("data/polygonData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  expect_equal(class(nc),"ncdf4")

  expect_equal(nc$dim$instance$vals,c(1))

  expect_equal(as.numeric(ncvar_get(nc,'x')),
               as.numeric(st_coordinates(polygonData)[,"X"]))

  expect_equal(as.numeric(ncvar_get(nc,'y')),
               as.numeric(st_coordinates(polygonData)[,"Y"]))

  expect_equal(as.numeric(ncvar_get(nc,'node_count')),
               nrow(st_coordinates(polygonData)))

  expect_equivalent(ncatt_get(nc, 0,"Conventions")$value,
                    pkg.env$cf_version)

  expect_equivalent(ncatt_get(nc, "x","standard_name")$value,
                    "longitude")
  expect_equivalent(ncatt_get(nc, "y","standard_name")$value,
                    "latitude")

  expect_equal(ncatt_get(nc, "x","axis")$value,
               pkg.env$x_axis)
  expect_equal(ncatt_get(nc, "y","axis")$value,
               pkg.env$y_axis)

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$node_coordinates)$value,
                    "x y")

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "polygon")

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$node_count_attr_name)$value,
                    pkg.env$node_count_var_name)

  expect_false(ncatt_get(nc, pkg.env$geom_container_var_name, "part_node_count")$hasatt)
  expect_false(ncatt_get(nc, pkg.env$geom_container_var_name, "part_type")$hasatt)

  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("polygon with a hole.", {
  polygonData <- readRDS("data/polygon_holeData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  expect_equal(nc$dim$instance$vals,c(1))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$node_count_var_name)),
               (nrow(st_geometry(polygonData)[[1]][[1]]) +
                	nrow(st_geometry(polygonData)[[1]][[2]])))

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$part_node_count_attr_name)$value,
                    pkg.env$part_node_count_var_name)

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$part_type_attr_name)$value,
                    pkg.env$part_type_var_name)

  expect_equal(as.numeric(sum(ncvar_get(nc, varid = pkg.env$part_node_count_var_name))),
               as.numeric(ncvar_get(nc, varid = pkg.env$node_count_var_name)))

  expect_equal(as.numeric(ncvar_get(nc, varid = pkg.env$part_type_var_name)),
               c(pkg.env$multi_val,pkg.env$hole_val))

  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("multipolygon.", {
  polygonData <- readRDS("data/multipolygonData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$node_count_var_name)),
  						 (nrow(st_geometry(polygonData)[[1]][[1]][[1]]) +
  						  	nrow(st_geometry(polygonData)[[1]][[2]][[1]])))

  expect_equal(as.numeric(ncvar_get(nc, pkg.env$part_node_count_var_name)),
               c(nrow(st_geometry(polygonData)[[1]][[1]][[1]]),
               		nrow(st_geometry(polygonData)[[1]][[2]][[1]])))

  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("multipolygon with a hole.", {
  polygonData <- readRDS("data/multipolygon_holeData.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$node_count_var_name)),
  						 (nrow(st_geometry(polygonData)[[1]][[1]][[1]]) +
  						  nrow(st_geometry(polygonData)[[1]][[2]][[1]]) +
  						  	nrow(st_geometry(polygonData)[[1]][[2]][[2]])))
  
  expect_equal(as.numeric(ncvar_get(nc,pkg.env$part_node_count_var_name)),
               c(nrow(st_geometry(polygonData)[[1]][[1]][[1]]),
               		nrow(st_geometry(polygonData)[[1]][[2]][[1]]),
               		nrow(st_geometry(polygonData)[[1]][[2]][[2]])))

  expect_equal(as.numeric(ncvar_get(nc, pkg.env$part_type_var_name)),
               c(pkg.env$multi_val, pkg.env$multi_val, pkg.env$hole_val))

  expect_equal(length(nc$dim$node$vals), as.numeric(sum(ncvar_get(nc,pkg.env$node_count_var_name))))
  expect_equal(length(nc$dim$node$vals), as.numeric(sum(ncvar_get(nc,pkg.env$part_node_count_var_name))))

  expect_equal(length(ncvar_get(nc,"x")), as.numeric(sum(ncvar_get(nc,pkg.env$node_count_var_name))))
  expect_equal(length(ncvar_get(nc,"x")), as.numeric(sum(ncvar_get(nc,pkg.env$part_node_count_var_name))))

  checkAllPoly(polygonData, ncvar_get(nc,pkg.env$node_count_var_name),
               ncvar_get(nc,pkg.env$part_node_count_var_name),
               ncvar_get(nc,pkg.env$part_type_var_name))

  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("multipolygons with holes.", {
  polygonData <- readRDS("data/multipolygons_holes.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc, pkg.env$part_type_var_name)),
               c(pkg.env$multi_val, pkg.env$hole_val, pkg.env$hole_val,
                 pkg.env$hole_val, pkg.env$multi_val, pkg.env$multi_val))

  expect_equal(length(nc$dim$node$vals), as.numeric(sum(ncvar_get(nc,pkg.env$node_count_var_name))))
  expect_equal(length(nc$dim$node$vals), as.numeric(sum(ncvar_get(nc,pkg.env$part_node_count_var_name))))

  expect_equal(length(ncvar_get(nc,"x")), as.numeric(sum(ncvar_get(nc,pkg.env$node_count_var_name))))
  expect_equal(length(ncvar_get(nc,"x")), as.numeric(sum(ncvar_get(nc,pkg.env$part_node_count_var_name))))

  checkAllPoly(polygonData, ncvar_get(nc,pkg.env$node_count_var_name),
               ncvar_get(nc,pkg.env$part_node_count_var_name),
               ncvar_get(nc,pkg.env$part_type_var_name))

  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
})
