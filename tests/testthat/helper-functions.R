library(RNetCDF)
library(ncdf4)
library(sf)

check_geom <- function(data, returndata) {
  
  expect_equal(nrow(data), nrow(returndata))
  
  expect_equal(sf::st_coordinates(returndata),
               sf::st_coordinates(data))
  
}

is_hole <- function(x) {
  one <- seq(1, nrow(x))
  two <- c(nrow(x), seq(1, (nrow(x) - 1)))
  
  sum((x[two, 1] - x[one, 1]) * (x[two, 2] + x[one, 2])) < 1
}

checkAllPoly <- function(polygonData, node_count, part_node_count = NULL, part_type = NULL) {
  
  geo <- sf::st_geometry(polygonData)
  
  i<-1 # counter for parts
  for(g in 1:length(geo)) {
    
    j<-0 # counter for coords in a geom
    
    for(p in 1:length(geo[[g]])) { # geometries
      
      if(is.list(geo[[g]][[p]]) & length(geo[[g]][[p]]) > 0) {
        
        for(mp in 1:length(geo[[g]][[p]])) {
          
          if(is_hole(geo[[g]][[p]][[mp]])) {
            expect_equal(part_type[i], pkg.env$hole_val)
          } else {
            expect_equal(part_type[i], pkg.env$multi_val) 
          }
          
          pCount <- nrow(geo[[g]][[p]][[mp]])
          
          expect_equal(part_node_count[i],
                       pCount)
          i <- i + 1
          j <- j + pCount
        }
      } else {
        
        if(is_hole(geo[[g]][[p]])) {
          expect_equal(part_type[i], pkg.env$hole_val)
        } else {
          expect_equal(part_type[i], pkg.env$multi_val)
        }
        
        pCount <- nrow(geo[[g]][[p]])
        
        expect_equal(part_node_count[i],
                     pCount)
        
        i <- i + 1
        j <- j + pCount
        
      }
      
    }
    
    expect_equal(nrow(st_coordinates(geo[[g]])), j)
  }
}

get_fixture_data <- function(geom_type) {
  fixtureData <- jsonlite::fromJSON(txt = system.file("extdata/fixture_wkt.json", 
                                                      package = "ncdfgeom"))
  
  return(sf::st_sf(geom = sf::st_as_sfc(fixtureData[["2d"]][geom_type]), 
                   crs = 4326))
}

get_sample_timeseries_data <- function() {
  
  yahara <- sf::read_sf(list.files(pattern = "Yahara_River_HRUs_alb_eq.shp", full.names = TRUE, recursive = TRUE))
  lon_lat <- yahara %>%
    sf::st_set_agr("constant") %>%
    sf::st_centroid() %>%
    sf::st_transform(4326) %>%
    sf::st_coordinates()
  
  lats<-lon_lat[,"Y"]
  lons<-lon_lat[,"X"]
  alts<-rep(1,length(lats))
  
  all_data <- readRDS(system.file('extdata/yahara_alb_gdp_file.rds', package = "ncdfgeom"))
  
  var_data <- all_data[2:(ncol(all_data)-3)]
  
  units <- all_data$units[1]
  
  time <- all_data$DateTime
  
  long_name=paste(all_data$variable[1], 'area weighted', 
                  all_data$statistic[1], 'in', 
                  all_data$units[1], sep=' ')
  
  meta<-list(name=all_data$variable[1],
             long_name=long_name)
  
  return(list(all_data = all_data,
              var_data = var_data,
              time = time,
              long_name = long_name,
              meta = meta,
              lons = lons,
              lats = lats,
              alts = alts,
              units = units,
              geom = yahara))
}

get_test_ncdf_object <- function(nc_file = tempfile()) {
  nc_summary<-'test summary'
  nc_date_create<-'2099-01-01'
  nc_creator_name='test creator'
  nc_creator_email='test@test.com'
  nc_project='testthat ncdfgeom'
  nc_proc_level='just a test no processing'
  nc_title<-'test title'
  global_attributes<-list(title = nc_title, summary = nc_summary, date_created=nc_date_create, 
                          creator_name=nc_creator_name,creator_email=nc_creator_email,
                          project=nc_project, processing_level=nc_proc_level)
  
  test_data <- get_sample_timeseries_data()
  
  testnc<-write_timeseries_dsg(nc_file, 
                               names(test_data$var_data), 
                               test_data$lats, test_data$lons, 
                               as.character(test_data$time), 
                               test_data$var, 
                               test_data$alts, 
                               data_unit=test_data$units,	
                               data_prec='double',
                               data_metadata=test_data$meta,
                               attributes=global_attributes)
  
  test_nc <- write_geometry(nc_file, test_data$geom, variables = test_data$meta$name)
  
  list(ncdfgeom = read_timeseries_dsg(nc_file), sf = read_geometry(nc_file))
}
