library("ncdf4")

context("NCDF SG polygonData tests")

# data prep.
# library(rgdal)
# shapeData<-readOGR(dsn = "data/Yahara_alb/Yahara_River_HRUs_alb_eq.shp",
#                    layer = "Yahara_River_HRUs_alb_eq",
#                    stringsAsFactors = FALSE)
# saveRDS(shapeData,file="data/yahara_shapefile_data.rds")

test_that("A whole shapefile can be written", {
  polygonData <- readRDS("data/yahara_shapefile_data.rds")
  nc_file <- ToNCDFSG(nc_file=tempfile(), geomData = polygonData)
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

  expect_equal(as.numeric(polygonData@data$GRIDCODE),as.numeric(ncvar_get(nc, varid = "GRIDCODE")))
  expect_equal(length(nc$dim$instance$vals), length(polygonData@polygons))

  for(var in names(polygonData@data)) {
    expect_equal(ncatt_get(nc, var, pkg.env$geometry_container_att_name)$value,
                 pkg.env$geom_container_var_name)
    expect_equal(ncatt_get(nc, var, pkg.env$crs)$value,
                 pkg.env$crs_var_name)
  }

  coords<-polygonData@polygons[[1]]@Polygons[[1]]@coords
  expect_equal(as.numeric(coords[nrow(coords):1,1]),as.numeric(ncvar_get(nc, varid = "x", start = c(1), count = c(118))))
  expect_equal(as.numeric(coords[nrow(coords):1,2]),as.numeric(ncvar_get(nc, varid = "y", start = c(1), count = c(118))))
  # Check to make sure a hole is encoded correctly.
  node_count <- ncvar_get(nc, pkg.env$node_count_var_name)
  part_node_count <- ncvar_get(nc, pkg.env$part_node_count_var_name)
  part_type <- ncvar_get(nc, pkg.env$part_type_var_name)
  expect_equal(length(polygonData@polygons), length(node_count))
  p <- 1
  for(i in 1:length(node_count)) {
    nCount <- 0
    for(j in 1:length(polygonData@polygons[[i]]@Polygons)) {
      if(polygonData@polygons[[i]]@Polygons[[j]]@hole) expect_equal(part_type[p], pkg.env$hole_val)
      expect_equal(length(polygonData@polygons[[i]]@Polygons[[j]]@coords[,1]), part_node_count[p])
      nCount <- nCount + part_node_count[p]
      p <- p + 1
    }
    expect_equal(nCount, node_count[i])
  }

  checkAllPoly(polygonData, ncvar_get(nc,pkg.env$node_count_var_name),
               ncvar_get(nc,pkg.env$part_node_count_var_name),
               ncvar_get(nc,pkg.env$part_type_var_name))

  returnPolyData<-FromNCDFSG(nc_file)
  compareSP(polygonData, returnPolyData)
  for(name in names(polygonData@data)) {
    expect_equal(as.character(polygonData@data[name]), as.character(returnPolyData@data[name]))
  }
  for(i in 1:length(returnPolyData@polygons)) {
    expect_equal(length(returnPolyData@polygons[[i]]@Polygons), length(polygonData@polygons[[i]]@Polygons))
    for(j in 1:length(returnPolyData@polygons[[i]]@Polygons)) {
      expect_equal(length(returnPolyData@polygons[[i]]@Polygons[[j]]@coords), length(polygonData@polygons[[i]]@Polygons[[j]]@coords))
    }
  }
  # writePolyShape(returnPolyData, "yaharaData_test")
})

