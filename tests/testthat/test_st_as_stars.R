context("st_as_stars tests")

test_that("basic st_as_stars", {
  test_list <- get_test_ncdf_object()
  
  stars_obj <- st_as_stars(test_list$ncdfgeom)
  
  expect_s3_class(stars_obj, "stars")
  
  dim <- stars::st_dimensions(stars_obj)
  expect_equal(sf::st_crs(dim$points$refsys), sf::st_crs(4326))
  expect_equal(dim$time$refsys, "POSIXct")
  
  expect_s3_class(dim$points$values, "sfc_POINT")
  
  expect_true(dim$points$point)  
  
  stars_obj <- st_as_stars(test_list$ncdfgeom, sf_geometry = test_list$sf)
  
  dim <- stars::st_dimensions(stars_obj)
  expect_equal(sf::st_crs(dim$geometry$refsys), sf::st_crs(test_list$sf))
  
  expect_s3_class(dim$geometry$values, "sfc_POLYGON")
  
  expect_false(dim$geometry$point)  
  
})