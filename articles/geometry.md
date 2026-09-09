# Reading and Writing NetCDF-CF Geometry

### Introduction

This example shows how to

1.  add polygon data to a NetCDF-CF Discrete Sampling Geometry
    timeSeries featureType file,
2.  read the polygon data from the NetCDF file,
3.  and write it to a standard GIS format.

Below the example, all the geometry types that `ncdfgeom` handles are
shown in detail.

### Load Spatial Data

Tables of point, line, or polygon features with associated timeseries
are the target for this functionality. Here, we use some sample data
from the `ncdfgeom` package.

``` r

example_file <- tempfile()

file.copy(from = system.file('extdata/example_huc_eta.nc', package = 'ncdfgeom'), 
                    to = example_file, 
                    overwrite = TRUE) -> quiet

polygons <- sf::read_sf(system.file('extdata/example_huc_eta.json', package = 'ncdfgeom'))

polygons <- dplyr::select(polygons, LOADDATE, AREASQKM, HUC12, NAME)

plot(sf::st_geometry(polygons))
```

![](geometry_files/figure-html/libs-1.png)

Now we have the polygons as shown above and a NetCDF file with a header
that looks like:

Now we can use the `write_geometry` function to add the polygon data to
the NetCDF file.

``` r

(vars <- ncmeta::nc_vars(example_file))
#> # A tibble: 5 × 5
#>      id name         type      ndims natts
#>   <int> <chr>        <chr>     <int> <int>
#> 1     0 lat          NC_DOUBLE     1     4
#> 2     1 lon          NC_DOUBLE     1     4
#> 3     2 time         NC_DOUBLE     1     4
#> 4     3 station_name NC_CHAR       2     5
#> 5     4 et           NC_INT        2     4

ncdfgeom::write_geometry(nc_file=example_file,
                         geom_data = polygons, 
                         instance_dim_name = "station", 
                         variables = vars$name) -> example_file
```

Now the NetCDF file looks like:

Read the polygon data from the file and write it out to a geopackage.

``` r

polygons_sf <- ncdfgeom::read_geometry(example_file)

plot(sf::st_geometry(polygons_sf))
```

![](geometry_files/figure-html/read-1.png)

``` r

sf::write_sf(polygons_sf, "polygons.gpkg")
```

### Geometry Types

## Examples - Contiguous Ragged Arrays

### Point (2D)

Well Known Text (WKT): `POINT (30 10)`  
Common Data Language (CDL):

    netcdf sample_point {
    dimensions:
        instance = 1 ;
    variables:
        double x_nodes(instance) ;
            x_nodes:units = "degrees_east" ;
            x_nodes:axis = "X" ;
        double y_nodes(instance) ;
            y_nodes:units = "degrees_north" ;
            y_nodes:axis = "Y" ;
        int geometry_container ;
            geometry_container:node_coordinates = "x_nodes y_nodes" ;
            geometry_container:geometry_type = "point" ;
            geometry_container:grid_mapping = "grid_mapping" ;
        int grid_mapping ;
            grid_mapping:grid_mapping_name = "latitude_longitude" ;
            grid_mapping:semi_major_axis = 6378137. ;
            grid_mapping:inverse_flattening = 298.257223563 ;
            grid_mapping:longitude_of_prime_meridian = 0. ;

    // global attributes:
            :Conventions = "CF-1.8" ;
    data:

     x_nodes = 30 ;

     y_nodes = 10 ;

     geometry_container = _ ;

     grid_mapping = _ ;
    }
      

### LineString (2D)

Well Known Text (WKT): `LINESTRING (30 10, 10 30, 40 40)`  
Common Data Language (CDL):

    netcdf sample_linestring {
    dimensions:
        node = 3 ;
        instance = 1 ;
    variables:
        double x_nodes(node) ;
            x_nodes:units = "degrees_east" ;
            x_nodes:axis = "X" ;
        double y_nodes(node) ;
            y_nodes:units = "degrees_north" ;
            y_nodes:axis = "Y" ;
        int geometry_container ;
            geometry_container:node_coordinates = "x_nodes y_nodes" ;
            geometry_container:geometry_type = "line" ;
            geometry_container:node_count = "node_count" ;
            geometry_container:grid_mapping = "grid_mapping" ;
        int node_count(instance) ;
            node_count:long_name = "count of coordinates in each instance geometry" ;
        int grid_mapping ;
            grid_mapping:grid_mapping_name = "latitude_longitude" ;
            grid_mapping:semi_major_axis = 6378137. ;
            grid_mapping:inverse_flattening = 298.257223563 ;
            grid_mapping:longitude_of_prime_meridian = 0. ;

    // global attributes:
            :Conventions = "CF-1.8" ;
    data:

     x_nodes = 30, 10, 40 ;

     y_nodes = 10, 30, 40 ;

     geometry_container = _ ;

     node_count = 3 ;

     grid_mapping = _ ;
    }
      

### Polygon (2D)

Well Known Text (WKT): `POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))`  
Common Data Language (CDL):

    netcdf sample_polygon {
    dimensions:
        node = 5 ;
        instance = 1 ;
    variables:
        double x_nodes(node) ;
            x_nodes:units = "degrees_east" ;
            x_nodes:axis = "X" ;
        double y_nodes(node) ;
            y_nodes:units = "degrees_north" ;
            y_nodes:axis = "Y" ;
        int geometry_container ;
            geometry_container:node_coordinates = "x_nodes y_nodes" ;
            geometry_container:geometry_type = "polygon" ;
            geometry_container:node_count = "node_count" ;
            geometry_container:grid_mapping = "grid_mapping" ;
        int node_count(instance) ;
            node_count:long_name = "count of coordinates in each instance geometry" ;
        int grid_mapping ;
            grid_mapping:grid_mapping_name = "latitude_longitude" ;
            grid_mapping:semi_major_axis = 6378137. ;
            grid_mapping:inverse_flattening = 298.257223563 ;
            grid_mapping:longitude_of_prime_meridian = 0. ;

    // global attributes:
            :Conventions = "CF-1.8" ;
    data:

     x_nodes = 30, 40, 20, 10, 30 ;

     y_nodes = 10, 40, 40, 20, 10 ;

     geometry_container = _ ;

     node_count = 5 ;

     grid_mapping = _ ;
    }
      

### MultiLineString (2D)

Well Known Text (WKT):
`MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))`  
Common Data Language (CDL):

    netcdf sample_multilinestring {
    dimensions:
        node = 7 ;
        instance = 1 ;
        part = 2 ;
    variables:
        double x_nodes(node) ;
            x_nodes:units = "degrees_east" ;
            x_nodes:axis = "X" ;
        double y_nodes(node) ;
            y_nodes:units = "degrees_north" ;
            y_nodes:axis = "Y" ;
        int geometry_container ;
            geometry_container:node_coordinates = "x_nodes y_nodes" ;
            geometry_container:geometry_type = "line" ;
            geometry_container:node_count = "node_count" ;
            geometry_container:part_node_count = "part_node_count" ;
            geometry_container:grid_mapping = "grid_mapping" ;
        int node_count(instance) ;
            node_count:long_name = "count of coordinates in each instance geometry" ;
        int part_node_count(part) ;
            part_node_count:long_name = "count of nodes in each geometry part" ;
        int grid_mapping ;
            grid_mapping:grid_mapping_name = "latitude_longitude" ;
            grid_mapping:semi_major_axis = 6378137. ;
            grid_mapping:inverse_flattening = 298.257223563 ;
            grid_mapping:longitude_of_prime_meridian = 0. ;

    // global attributes:
            :Conventions = "CF-1.8" ;
    data:

     x_nodes = 10, 20, 10, 40, 30, 40, 30 ;

     y_nodes = 10, 20, 40, 40, 30, 20, 10 ;

     geometry_container = _ ;

     node_count = 7 ;

     part_node_count = 3, 4 ;

     grid_mapping = _ ;
    }
      

### MultiPolygon (2D)

Well Known Text (WKT):
`MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)), ((15 5, 40 10, 10 20, 5 10, 15 5)))`  
Common Data Language (CDL):

    netcdf sample_multipolygon {
    dimensions:
        node = 9 ;
        instance = 1 ;
        part = 2 ;
    variables:
        double x_nodes(node) ;
            x_nodes:units = "degrees_east" ;
            x_nodes:axis = "X" ;
        double y_nodes(node) ;
            y_nodes:units = "degrees_north" ;
            y_nodes:axis = "Y" ;
        int geometry_container ;
            geometry_container:node_coordinates = "x_nodes y_nodes" ;
            geometry_container:geometry_type = "polygon" ;
            geometry_container:node_count = "node_count" ;
            geometry_container:interior_ring = "interior_ring" ;
            geometry_container:grid_mapping = "grid_mapping" ;
            geometry_container:part_node_count = "part_node_count" ;
        int node_count(instance) ;
            node_count:long_name = "count of coordinates in each instance geometry" ;
        int part_node_count(part) ;
            part_node_count:long_name = "count of nodes in each geometry part" ;
        int interior_ring(part) ;
            interior_ring:long_name = "type of each geometry part" ;
        int grid_mapping ;
            grid_mapping:grid_mapping_name = "latitude_longitude" ;
            grid_mapping:semi_major_axis = 6378137. ;
            grid_mapping:inverse_flattening = 298.257223563 ;
            grid_mapping:longitude_of_prime_meridian = 0. ;

    // global attributes:
            :Conventions = "CF-1.8" ;
    data:

     x_nodes = 30, 45, 10, 30, 15, 40, 10, 5, 15 ;

     y_nodes = 20, 40, 40, 20, 5, 10, 20, 10, 5 ;

     geometry_container = _ ;

     node_count = 9 ;

     part_node_count = 4, 5 ;

     interior_ring = 0, 0 ;

     grid_mapping = _ ;
    }
      

### Polygon with One Interior Ring (2D)

Well Known Text (WKT):
`POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))`  
Common Data Language (CDL):

    netcdf sample_polygon_hole {
    dimensions:
        node = 9 ;
        instance = 1 ;
        part = 2 ;
    variables:
        double x_nodes(node) ;
            x_nodes:units = "degrees_east" ;
            x_nodes:axis = "X" ;
        double y_nodes(node) ;
            y_nodes:units = "degrees_north" ;
            y_nodes:axis = "Y" ;
        int geometry_container ;
            geometry_container:node_coordinates = "x_nodes y_nodes" ;
            geometry_container:geometry_type = "polygon" ;
            geometry_container:node_count = "node_count" ;
            geometry_container:interior_ring = "interior_ring" ;
            geometry_container:grid_mapping = "grid_mapping" ;
            geometry_container:part_node_count = "part_node_count" ;
        int node_count(instance) ;
            node_count:long_name = "count of coordinates in each instance geometry" ;
        int part_node_count(part) ;
            part_node_count:long_name = "count of nodes in each geometry part" ;
        int interior_ring(part) ;
            interior_ring:long_name = "type of each geometry part" ;
        int grid_mapping ;
            grid_mapping:grid_mapping_name = "latitude_longitude" ;
            grid_mapping:semi_major_axis = 6378137. ;
            grid_mapping:inverse_flattening = 298.257223563 ;
            grid_mapping:longitude_of_prime_meridian = 0. ;

    // global attributes:
            :Conventions = "CF-1.8" ;
    data:

     x_nodes = 35, 45, 15, 10, 35, 20, 35, 30, 20 ;

     y_nodes = 10, 45, 40, 20, 10, 30, 35, 20, 30 ;

     geometry_container = _ ;

     node_count = 9 ;

     part_node_count = 5, 4 ;

     interior_ring = 0, 1 ;

     grid_mapping = _ ;
    }
      

### MultiPolygon with One Interior Ring (2D)

Well Known Text (WKT):
`MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)), ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))`  
Common Data Language (CDL):

    netcdf sample_multipolygon_hole {
    dimensions:
        node = 14 ;
        instance = 1 ;
        part = 3 ;
    variables:
        double x_nodes(node) ;
            x_nodes:units = "degrees_east" ;
            x_nodes:axis = "X" ;
        double y_nodes(node) ;
            y_nodes:units = "degrees_north" ;
            y_nodes:axis = "Y" ;
        int geometry_container ;
            geometry_container:node_coordinates = "x_nodes y_nodes" ;
            geometry_container:geometry_type = "polygon" ;
            geometry_container:node_count = "node_count" ;
            geometry_container:interior_ring = "interior_ring" ;
            geometry_container:grid_mapping = "grid_mapping" ;
            geometry_container:part_node_count = "part_node_count" ;
        int node_count(instance) ;
            node_count:long_name = "count of coordinates in each instance geometry" ;
        int part_node_count(part) ;
            part_node_count:long_name = "count of nodes in each geometry part" ;
        int interior_ring(part) ;
            interior_ring:long_name = "type of each geometry part" ;
        int grid_mapping ;
            grid_mapping:grid_mapping_name = "latitude_longitude" ;
            grid_mapping:semi_major_axis = 6378137. ;
            grid_mapping:inverse_flattening = 298.257223563 ;
            grid_mapping:longitude_of_prime_meridian = 0. ;

    // global attributes:
            :Conventions = "CF-1.8" ;
    data:

     x_nodes = 40, 20, 45, 40, 20, 10, 10, 30, 45, 20, 30, 20, 20, 30 ;

     y_nodes = 40, 45, 30, 40, 35, 30, 10, 5, 20, 35, 20, 15, 25, 20 ;

     geometry_container = _ ;

     node_count = 14 ;

     part_node_count = 4, 6, 4 ;

     interior_ring = 0, 0, 1 ;

     grid_mapping = _ ;
    }
      

### Multiple MultiPolygons with Interior Rings (2D)

Well Known Text (WKT):
`MULTIPOLYGON(((0 0, 20 0, 20 20, 0 20, 0 0), (1 1, 10 5, 19 1, 1 1), (5 15, 7 19, 9 15, 5 15), (11 15, 13 19, 15 15, 11 15)), ((5 25, 9 25, 7 29, 5 25)), ((11 25, 15 25, 13 29, 11 25)))`  
Common Data Language (CDL):

    netcdf sample_multipolygons_holes {
    dimensions:
        node = 25 ;
        instance = 1 ;
        part = 6 ;
    variables:
        double x_nodes(node) ;
            x_nodes:units = "degrees_east" ;
            x_nodes:axis = "X" ;
        double y_nodes(node) ;
            y_nodes:units = "degrees_north" ;
            y_nodes:axis = "Y" ;
        int geometry_container ;
            geometry_container:node_coordinates = "x_nodes y_nodes" ;
            geometry_container:geometry_type = "polygon" ;
            geometry_container:node_count = "node_count" ;
            geometry_container:interior_ring = "interior_ring" ;
            geometry_container:grid_mapping = "grid_mapping" ;
            geometry_container:part_node_count = "part_node_count" ;
        int node_count(instance) ;
            node_count:long_name = "count of coordinates in each instance geometry" ;
        int part_node_count(part) ;
            part_node_count:long_name = "count of nodes in each geometry part" ;
        int interior_ring(part) ;
            interior_ring:long_name = "type of each geometry part" ;
        int grid_mapping ;
            grid_mapping:grid_mapping_name = "latitude_longitude" ;
            grid_mapping:semi_major_axis = 6378137. ;
            grid_mapping:inverse_flattening = 298.257223563 ;
            grid_mapping:longitude_of_prime_meridian = 0. ;

    // global attributes:
            :Conventions = "CF-1.8" ;
    data:

     x_nodes = 0, 20, 20, 0, 0, 1, 10, 19, 1, 5, 7, 9, 5, 11, 13, 15, 11, 5, 9, 
        7, 5, 11, 15, 13, 11 ;

     y_nodes = 0, 0, 20, 20, 0, 1, 5, 1, 1, 15, 19, 15, 15, 15, 19, 15, 15, 25, 
        25, 29, 25, 25, 25, 29, 25 ;

     geometry_container = _ ;

     node_count = 25 ;

     part_node_count = 5, 4, 4, 4, 4, 4 ;

     interior_ring = 0, 1, 1, 1, 0, 0 ;

     grid_mapping = _ ;
    }
      
