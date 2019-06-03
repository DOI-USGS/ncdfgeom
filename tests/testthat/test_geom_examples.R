context("geom_examples.md")

test_that("create geom_examples.md", {
  geom_examples <- "data/geom_examples.md"
  
  fixtureData <- jsonlite::fromJSON(system.file('extdata/fixture_wkt.json', package = 'ncdfgeom'))
  # "multipoint", "MultiPoint (2D)", 
  order<-c("point", "linestring", "polygon",
           "multilinestring", "multipolygon",
           "polygon_hole", "multipolygon_hole", "multipolygons_holes")
  
  namesstr<-c("Point (2D)", "LineString (2D)", "Polygon (2D)",
              "MultiLineString (2D)", "MultiPolygon (2D)",
              "Polygon with One Interior Ring (2D)", "MultiPolygon with One Interior Ring (2D)",
              "Multiple MultiPolygons with Interior Rings (2D)")
  
  sink(geom_examples)
  cat(paste("# Examples - Contiguous Ragged Arrays  \n\n"))
  
  try({
  for(geom in 1:length(namesstr)) {
    cat(paste0("## ", namesstr[geom],"  \nWell Known Text (WKT): ```",fixtureData[["2d"]][order[geom]]),"```  \n")
    fileName<-paste0("sample_",order[geom],".nc")
    if(grepl('point',order[geom])) {
      write_geometry(fileName, get_fixture_data(order[geom]))
    } else {
      write_geometry(fileName, get_fixture_data(order[geom]))
    }
    cat("Common Data Language (CDL):\n```  \n")
    t <- system(paste0("ncdump ", fileName), intern = TRUE)
    cat(t, sep = "\n")
    cat("  \n```  \n\n")
    system(paste("rm", fileName))
  }
  sink()
  testthat::skip_on_cran()
  expect_true(file.exists(geom_examples))
  }, silent = TRUE)
})
