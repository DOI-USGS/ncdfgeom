context("checkncdf")

test_that("line", {
  lineData <- get_fixture_data("linestring")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = lineData)

  checkVals <- ncdfgeom:::check_netcdf(nc_file)

  expect_equal(checkVals$instance_id, NULL)
  expect_equal(checkVals$instance_dim, pkg.env$instance_dim_name)
  expect_equal(checkVals$geom_container$geom_type, "line")
  expect_equal(checkVals$geom_container$node_count, pkg.env$node_count_var_name)
  expect_equal(checkVals$geom_container$part_node_count, character(0))
  expect_equal(checkVals$geom_container$part_type, character(0))
  expect_equal(checkVals$geom_container$x, pkg.env$x_nodes)
  expect_equal(checkVals$geom_container$y, pkg.env$y_nodes)
})

test_that("line", {
  lineData <- get_fixture_data("multilinestring")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = lineData)

  checkVals <- ncdfgeom:::check_netcdf(nc_file)
  expect_equal(checkVals$geom_container$geom_type, "line")
  expect_equal(checkVals$geom_container$node_count, pkg.env$node_count_var_name)
  expect_equal(checkVals$geom_container$part_node_count, pkg.env$part_node_count_var_name)
  expect_equal(checkVals$geom_container$part_type, character(0))
})

test_that("multi polygon holes", {
  polygonData <- get_fixture_data("multipolygons_holes")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  checkVals <- ncdfgeom:::check_netcdf(nc_file)

  expect_equal(checkVals$geom_container$geom_type, "polygon")
  expect_equal(checkVals$geom_container$node_count, pkg.env$node_count_var_name)
  expect_equal(checkVals$geom_container$part_node_count, pkg.env$part_node_count_var_name)
  expect_equal(checkVals$geom_container$part_type, pkg.env$part_type_var_name)
})

test_that("polygon", {
  polygonData <- get_fixture_data("polygon")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)

  checkVals <- ncdfgeom:::check_netcdf(nc_file)

  expect_equal(checkVals$geom_container$geom_type, "polygon")
  expect_equal(checkVals$geom_container$node_count, pkg.env$node_count_var_name)
  expect_equal(checkVals$geom_container$part_node_count, character(0))
  expect_equal(checkVals$geom_container$part_type, character(0))
})

test_that("basic point works", {
  pointData <- get_fixture_data("point")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = pointData)
  nc<-nc_open(nc_file)

  checkVals <- ncdfgeom:::check_netcdf(nc_file)

  expect_equal(checkVals$instance_dim, pkg.env$instance_dim_name)
})

test_that("a crs gets found correctly", {
  polygonData <- sf::read_sf("data/Yahara_alb/Yahara_River_HRUs_alb_eq.shp")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = polygonData)
  nc<-nc_open(nc_file)
  checkVals <- ncdfgeom:::check_netcdf(nc_file)
  crs <- list(grid_mapping_name = "albers_conical_equal_area",
              longitude_of_central_meridian = -96,
              latitude_of_projection_origin = 23,
              false_easting = 0.0,
              false_northing = 0.0,
              standard_parallel = c(29.5, 45.5),
              semi_major_axis = 6378137.0,
              inverse_flattening = 298.257223563,
              longitude_of_prime_meridian = 0)
  expect_equal(checkVals$crs, crs)
})

test_that("errors", {
  pointData <- get_fixture_data("point")
  nc_file <- write_geometry(nc_file=tempfile(), geomData = pointData)
  nc <- nc_open(nc_file, write = TRUE)
  ncatt_put(nc, nc$var$y, pkg.env$geom_type_attr_name, "garbage")
  nc_close(nc)
  expect_error(ncdfgeom:::check_netcdf(nc_file), "only one geometry container per file supported")

  nc_file <- write_geometry(nc_file=tempfile(), geomData = pointData)
  nc <- nc_open(nc_file, write = TRUE)
  ncatt_put(nc, 0,"Conventions", "garbage")
  expect_warning(ncdfgeom:::check_netcdf(nc_file), 'File does not advertise CF conventions, unexpected behavior may result.')
  nc_close(nc)
  
  nc <- open.nc(nc_file, write = TRUE)
  RNetCDF::att.put.nc(nc, "NC_GLOBAL", "Conventions", "NC_CHAR", "CF-1.8")
  RNetCDF::att.delete.nc(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)
  close.nc(nc)
  expect_error(ncdfgeom:::check_netcdf(nc_file), "Didn't find a geometry type attribute, nothing to do.")

  nc <- open.nc(nc_file, write = TRUE)
  RNetCDF::att.put.nc(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name, "NC_CHAR", "line")
  RNetCDF::att.put.nc(nc, pkg.env$x_nodes, "axis", "NC_CHAR", "bork")
  close.nc(nc)
  expect_error(ncdfgeom:::check_netcdf(nc_file), "unexpected axis attribute X and Y are allowed.")
  
  nc <- open.nc(nc_file, write = TRUE)
  RNetCDF::att.put.nc(nc, pkg.env$x_nodes, "axis", "NC_CHAR", pkg.env$x_axis)
  RNetCDF::att.put.nc(nc, pkg.env$x_nodes, "grid_mapping", "NC_CHAR", "bork")
  close.nc(nc)
  expect_warning(ncdfgeom:::check_netcdf(nc_file), "Only one crs is supported, more than one was found, may be handling projections wrong.")
  
  nc_file <- "data/temp.nc"
  
  file.copy(from = system.file('extdata/example_huc_eta.nc', package = 'ncdfgeom'), 
            to = nc_file, 
            overwrite = TRUE) -> quiet
  
  vars <- ncmeta::nc_vars(nc_file)
  
  hucPolygons <- sf::read_sf(system.file('extdata/example_huc_eta.json', package = 'ncdfgeom'))
  
  ncdfgeom::write_geometry(nc_file=nc_file,
                           geomData = hucPolygons, 
                           instance_dim_name = "station", 
                           variables = vars$name) -> nc_file
  
  nc <- open.nc(nc_file, write = TRUE)
  att.put.nc(nc, "lon", "cf_role", "NC_CHAR", "timeseries_id")
  close.nc(nc)
  
  expect_error(ncdfgeom:::check_netcdf(nc_file), "multiple timeseries id variables were found.")
  
  unlink(nc_file)
})
