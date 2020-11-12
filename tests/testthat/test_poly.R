
context("polygon")

test_that("data for basic polygon", {
  polygonData <- get_fixture_data("polygon")
  nc_file <- write_geometry(nc_file=tempfile(), geom_data = polygonData)
  nc<-nc_open(nc_file)

  expect_equal(nc$dim$instance$vals,c(1))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$x_nodes)),
               as.numeric(st_coordinates(polygonData)[,"X"]))

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$y_nodes)),
               as.numeric(st_coordinates(polygonData)[,"Y"]))

  expect_equal(as.numeric(ncvar_get(nc,'node_count')),
               nrow(st_coordinates(polygonData)))

  expect_equivalent(ncatt_get(nc, 0,"Conventions")$value,
                    pkg.env$cf_version)

  expect_equal(ncatt_get(nc, pkg.env$x_nodes,"axis")$value,
               pkg.env$x_axis)
  expect_equal(ncatt_get(nc, pkg.env$y_nodes,"axis")$value,
               pkg.env$y_axis)

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$node_coordinates)$value,
                    paste(pkg.env$x_nodes, pkg.env$y_nodes))

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$geom_type_attr_name)$value,
                    "polygon")

  expect_equivalent(ncatt_get(nc, pkg.env$geom_container_var_name, pkg.env$node_count_attr_name)$value,
                    pkg.env$node_count_var_name)

  expect_false(ncatt_get(nc, pkg.env$geom_container_var_name, "part_node_count")$hasatt)
  expect_false(ncatt_get(nc, pkg.env$geom_container_var_name, "part_type")$hasatt)

  returnPolyData<-read_geometry(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("polygon with a hole.", {
  polygonData <- get_fixture_data("polygon_hole")
  nc_file <- write_geometry(nc_file=tempfile(), geom_data = polygonData)
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

  returnPolyData<-read_geometry(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("multipolygon.", {
  polygonData <- get_fixture_data("multipolygon")
  nc_file <- write_geometry(nc_file=tempfile(), geom_data = polygonData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc,pkg.env$node_count_var_name)),
  						 (nrow(st_geometry(polygonData)[[1]][[1]][[1]]) +
  						  	nrow(st_geometry(polygonData)[[1]][[2]][[1]])))

  expect_equal(as.numeric(ncvar_get(nc, pkg.env$part_node_count_var_name)),
               c(nrow(st_geometry(polygonData)[[1]][[1]][[1]]),
               		nrow(st_geometry(polygonData)[[1]][[2]][[1]])))

  returnPolyData<-read_geometry(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("multipolygon with a hole.", {
  polygonData <-get_fixture_data("multipolygon_hole")
  nc_file <- write_geometry(nc_file=tempfile(), geom_data = polygonData)
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

  expect_equal(length(ncvar_get(nc,pkg.env$x_nodes)), as.numeric(sum(ncvar_get(nc,pkg.env$node_count_var_name))))
  expect_equal(length(ncvar_get(nc,pkg.env$x_nodes)), as.numeric(sum(ncvar_get(nc,pkg.env$part_node_count_var_name))))

  checkAllPoly(polygonData, ncvar_get(nc,pkg.env$node_count_var_name),
               ncvar_get(nc,pkg.env$part_node_count_var_name),
               ncvar_get(nc,pkg.env$part_type_var_name))

  returnPolyData<-read_geometry(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("multipolygons with holes.", {
  polygonData <- get_fixture_data("multipolygons_holes")
  nc_file <- write_geometry(nc_file=tempfile(), geom_data = polygonData)
  nc<-nc_open(nc_file)

  expect_equal(as.numeric(ncvar_get(nc, pkg.env$part_type_var_name)),
               c(pkg.env$multi_val, pkg.env$hole_val, pkg.env$hole_val,
                 pkg.env$hole_val, pkg.env$multi_val, pkg.env$multi_val))

  expect_equal(length(nc$dim$node$vals), as.numeric(sum(ncvar_get(nc,pkg.env$node_count_var_name))))
  expect_equal(length(nc$dim$node$vals), as.numeric(sum(ncvar_get(nc,pkg.env$part_node_count_var_name))))

  expect_equal(length(ncvar_get(nc,pkg.env$x_nodes)), as.numeric(sum(ncvar_get(nc,pkg.env$node_count_var_name))))
  expect_equal(length(ncvar_get(nc,pkg.env$x_nodes)), as.numeric(sum(ncvar_get(nc,pkg.env$part_node_count_var_name))))

  checkAllPoly(polygonData, ncvar_get(nc,pkg.env$node_count_var_name),
               ncvar_get(nc,pkg.env$part_node_count_var_name),
               ncvar_get(nc,pkg.env$part_type_var_name))

  returnPolyData<-read_geometry(nc_file)
  compareSP(polygonData, returnPolyData)
})

test_that("A whole shapefile can be written", {
  polygonData <- read_sf("data/Yahara_alb/Yahara_River_HRUs_alb_eq.shp")
  nc_file <- write_geometry(nc_file=tempfile(), geom_data = polygonData)
  nc<-nc_open(nc_file)
  
  crs <- list(grid_mapping_name = "albers_conical_equal_area",
              longitude_of_central_meridian = -96,
              latitude_of_projection_origin = 23,
              false_easting = 0.0,
              false_northing = 0.0,
              standard_parallel = c(29.5, 45.5),
              semi_major_axis = 6378137.0,
              inverse_flattening = 298.257223563,
              longitude_of_prime_meridian = 0)
  
  expect_equal(ncatt_get(nc, pkg.env$crs_var_name)[names(crs)], crs)
  
  expect_equal(as.numeric(polygonData$GRIDCODE),as.numeric(ncvar_get(nc, varid = "GRIDCODE")))
  expect_equal(length(nc$dim$instance$vals), nrow(polygonData))
  
  for(var in names(sf::st_set_geometry(polygonData, NULL))) {
    expect_equal(ncatt_get(nc, var, pkg.env$geometry_container_att_name)$value,
                 pkg.env$geom_container_var_name)
    expect_equal(ncatt_get(nc, var, pkg.env$crs)$value,
                 pkg.env$crs_var_name)
  }
  
  coords<-sf::st_coordinates(sf::st_geometry(polygonData)[[1]])[, c("X", "Y")]
  expect_equal(as.numeric(coords[1:nrow(coords),1]),
               as.numeric(ncvar_get(nc, varid = pkg.env$x_nodes, start = c(1), count = c(118))))
  expect_equal(as.numeric(coords[1:nrow(coords),2]),
               as.numeric(ncvar_get(nc, varid = pkg.env$y_nodes, start = c(1), count = c(118))))
  # Check to make sure a hole is encoded correctly.
  node_count <- ncvar_get(nc, pkg.env$node_count_var_name)
  part_node_count <- ncvar_get(nc, pkg.env$part_node_count_var_name)
  part_type <- ncvar_get(nc, pkg.env$part_type_var_name)
  expect_equal(nrow(polygonData), length(node_count))
  
  polygonData_sp <- sf::as_Spatial(polygonData)
  p <- 1
  for(i in 1:length(node_count)) {
    nCount <- 0
    for(j in 1:length(polygonData_sp@polygons[[i]]@Polygons)) {
      if(polygonData_sp@polygons[[i]]@Polygons[[j]]@hole) expect_equal(part_type[p], pkg.env$hole_val)
      expect_equal(length(polygonData_sp@polygons[[i]]@Polygons[[j]]@coords[,1]), part_node_count[p])
      nCount <- nCount + part_node_count[p]
      p <- p + 1
    }
    expect_equal(nCount, node_count[i])
  }
  
  checkAllPoly(polygonData, ncvar_get(nc,pkg.env$node_count_var_name),
               ncvar_get(nc,pkg.env$part_node_count_var_name),
               ncvar_get(nc,pkg.env$part_type_var_name))
  
  returnPolyData<-read_geometry(nc_file)
  compareSP(polygonData, returnPolyData)
  
  for(name in names(sf::st_set_geometry(polygonData, NULL))) {
    expect_equal(as.character(sf::st_set_geometry(polygonData[name], NULL)), 
                 as.character(sf::st_set_geometry(returnPolyData[name], NULL)))
  }
  
  returnPolyData <- dplyr::arrange(returnPolyData, ID)
  polygonData <- dplyr::arrange(polygonData, ID)
  for(i in 1:nrow(polygonData)) {
    expect_equal(st_coordinates(polygonData[i, ]), st_coordinates(returnPolyData[i, ]), info = paste("error in poly", i))
  }
})

test_that("big roundtrip", {
  skip_on_cran()
  nc_file <- list.files(pattern = "climdiv_prcp.nc", full.names = TRUE, recursive = TRUE)
  
  expect_warning(
    prcp_data <- read_timeseries_dsg(nc_file),
    "no altitude coordinate found")
  
  expect_equal(length(prcp_data), 9)
  expect_equal(length(prcp_data$time), 1500)
  expect_s3_class(prcp_data$time[1], "POSIXct")
  expect_equal(nrow(prcp_data$data_frames[[1]]), 1500)
  expect_equal(ncol(prcp_data$data_frames[[1]]), 344)
  
  climdiv_poly <- read_geometry(nc_file)
  
  expect_s3_class(climdiv_poly, "sf")
  expect_s3_class(climdiv_poly$geom, "sfc_GEOMETRY")

  out_nc <- write_timeseries_dsg(nc_file = tempfile(), 
                                 instance_names = names(prcp_data$data_frames[[1]]), 
                                 lats = prcp_data$lats,
                                 lons = prcp_data$lons, 
                                 times = prcp_data$time, 
                                 data = prcp_data$data_frames[[1]], 
                                 data_unit = prcp_data$data_unit[[1]], 
                                 data_prec = prcp_data$data_prec[[1]], 
                                 data_metadata = prcp_data$varmeta[[1]], 
                                 attributes = prcp_data$global_attributes[[1]], 
                                 overwrite = TRUE)
  
  expect_error(write_geometry(out_nc, climdiv_poly, variables = "climdiv_prcp_inches"),
               "Found multiple geometry types, only one is supported.")
  
  climdiv_poly <- st_sf(st_cast(climdiv_poly, "MULTIPOLYGON"))
  
  expect_warning(out_nc <- write_geometry(out_nc, climdiv_poly, variables = "climdiv_prcp_inches"),
                 "no datum information found assuming WGS84")
})