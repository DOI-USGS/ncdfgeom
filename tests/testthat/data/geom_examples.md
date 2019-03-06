# Examples - Contiguous Ragged Arrays  

## Point (2D)  
Well Known Text (WKT): ```POINT (30 10) ```  
Common Data Language (CDL):
```  
netcdf sample_point {
dimensions:
	instance = 1 ;
variables:
	double x_nodes(instance) ;
		x_nodes:units = "degrees_east" ;
		x_nodes:standard_name = "longitude" ;
		x_nodes:axis = "X" ;
	double y_nodes(instance) ;
		y_nodes:units = "degrees_north" ;
		y_nodes:standard_name = "latitude" ;
		y_nodes:axis = "Y" ;
	float geometry_container ;
		geometry_container:geometry_type = "point" ;
		geometry_container:node_coordinates = "x_nodes y_nodes" ;
		geometry_container:grid_mapping = "grid_mapping" ;
	float grid_mapping ;
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
  
```  

## LineString (2D)  
Well Known Text (WKT): ```LINESTRING (30 10, 10 30, 40 40) ```  
Common Data Language (CDL):
```  
netcdf sample_linestring {
dimensions:
	node = 3 ;
	instance = 1 ;
variables:
	double x_nodes(node) ;
		x_nodes:units = "degrees_east" ;
		x_nodes:standard_name = "longitude" ;
		x_nodes:axis = "X" ;
	double y_nodes(node) ;
		y_nodes:units = "degrees_north" ;
		y_nodes:standard_name = "latitude" ;
		y_nodes:axis = "Y" ;
	float geometry_container ;
		geometry_container:geometry_type = "line" ;
		geometry_container:node_count = "node_count" ;
		geometry_container:node_coordinates = "x_nodes y_nodes" ;
		geometry_container:grid_mapping = "grid_mapping" ;
	int node_count(instance) ;
		node_count:long_name = "count of coordinates in each instance geometry" ;
	float grid_mapping ;
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
  
```  

## Polygon (2D)  
Well Known Text (WKT): ```POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10)) ```  
Common Data Language (CDL):
```  
netcdf sample_polygon {
dimensions:
	node = 5 ;
	instance = 1 ;
variables:
	double x_nodes(node) ;
		x_nodes:units = "degrees_east" ;
		x_nodes:standard_name = "longitude" ;
		x_nodes:axis = "X" ;
	double y_nodes(node) ;
		y_nodes:units = "degrees_north" ;
		y_nodes:standard_name = "latitude" ;
		y_nodes:axis = "Y" ;
	float geometry_container ;
		geometry_container:geometry_type = "polygon" ;
		geometry_container:node_count = "node_count" ;
		geometry_container:node_coordinates = "x_nodes y_nodes" ;
		geometry_container:grid_mapping = "grid_mapping" ;
	int node_count(instance) ;
		node_count:long_name = "count of coordinates in each instance geometry" ;
	float grid_mapping ;
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
  
```  

## MultiLineString (2D)  
Well Known Text (WKT): ```MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10)) ```  
Common Data Language (CDL):
```  
netcdf sample_multilinestring {
dimensions:
	node = 7 ;
	instance = 1 ;
	part = 2 ;
variables:
	double x_nodes(node) ;
		x_nodes:units = "degrees_east" ;
		x_nodes:standard_name = "longitude" ;
		x_nodes:axis = "X" ;
	double y_nodes(node) ;
		y_nodes:units = "degrees_north" ;
		y_nodes:standard_name = "latitude" ;
		y_nodes:axis = "Y" ;
	float geometry_container ;
		geometry_container:geometry_type = "line" ;
		geometry_container:node_count = "node_count" ;
		geometry_container:node_coordinates = "x_nodes y_nodes" ;
		geometry_container:grid_mapping = "grid_mapping" ;
		geometry_container:part_node_count = "part_node_count" ;
	int node_count(instance) ;
		node_count:long_name = "count of coordinates in each instance geometry" ;
	float grid_mapping ;
		grid_mapping:grid_mapping_name = "latitude_longitude" ;
		grid_mapping:semi_major_axis = 6378137. ;
		grid_mapping:inverse_flattening = 298.257223563 ;
		grid_mapping:longitude_of_prime_meridian = 0. ;
	int part_node_count(part) ;
		part_node_count:long_name = "count of nodes in each geometry part" ;

// global attributes:
		:Conventions = "CF-1.8" ;
data:

 x_nodes = 10, 20, 10, 40, 30, 40, 30 ;

 y_nodes = 10, 20, 40, 40, 30, 20, 10 ;

 geometry_container = _ ;

 node_count = 7 ;

 grid_mapping = _ ;

 part_node_count = 3, 4 ;
}
  
```  

## MultiPolygon (2D)  
Well Known Text (WKT): ```MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)), ((15 5, 40 10, 10 20, 5 10, 15 5))) ```  
Common Data Language (CDL):
```  
netcdf sample_multipolygon {
dimensions:
	node = 9 ;
	instance = 1 ;
	part = 2 ;
variables:
	double x_nodes(node) ;
		x_nodes:units = "degrees_east" ;
		x_nodes:standard_name = "longitude" ;
		x_nodes:axis = "X" ;
	double y_nodes(node) ;
		y_nodes:units = "degrees_north" ;
		y_nodes:standard_name = "latitude" ;
		y_nodes:axis = "Y" ;
	float geometry_container ;
		geometry_container:geometry_type = "polygon" ;
		geometry_container:node_count = "node_count" ;
		geometry_container:node_coordinates = "x_nodes y_nodes" ;
		geometry_container:grid_mapping = "grid_mapping" ;
		geometry_container:part_node_count = "part_node_count" ;
	int node_count(instance) ;
		node_count:long_name = "count of coordinates in each instance geometry" ;
	float grid_mapping ;
		grid_mapping:grid_mapping_name = "latitude_longitude" ;
		grid_mapping:semi_major_axis = 6378137. ;
		grid_mapping:inverse_flattening = 298.257223563 ;
		grid_mapping:longitude_of_prime_meridian = 0. ;
	int part_node_count(part) ;
		part_node_count:long_name = "count of nodes in each geometry part" ;

// global attributes:
		:Conventions = "CF-1.8" ;
data:

 x_nodes = 30, 45, 10, 30, 15, 40, 10, 5, 15 ;

 y_nodes = 20, 40, 40, 20, 5, 10, 20, 10, 5 ;

 geometry_container = _ ;

 node_count = 9 ;

 grid_mapping = _ ;

 part_node_count = 4, 5 ;
}
  
```  

## Polygon with One Interior Ring (2D)  
Well Known Text (WKT): ```POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30)) ```  
Common Data Language (CDL):
```  
netcdf sample_polygon_hole {
dimensions:
	node = 9 ;
	instance = 1 ;
	part = 2 ;
variables:
	double x_nodes(node) ;
		x_nodes:units = "degrees_east" ;
		x_nodes:standard_name = "longitude" ;
		x_nodes:axis = "X" ;
	double y_nodes(node) ;
		y_nodes:units = "degrees_north" ;
		y_nodes:standard_name = "latitude" ;
		y_nodes:axis = "Y" ;
	float geometry_container ;
		geometry_container:geometry_type = "polygon" ;
		geometry_container:node_count = "node_count" ;
		geometry_container:node_coordinates = "x_nodes y_nodes" ;
		geometry_container:grid_mapping = "grid_mapping" ;
		geometry_container:part_node_count = "part_node_count" ;
		geometry_container:interior_ring = "interior_ring" ;
	int node_count(instance) ;
		node_count:long_name = "count of coordinates in each instance geometry" ;
	float grid_mapping ;
		grid_mapping:grid_mapping_name = "latitude_longitude" ;
		grid_mapping:semi_major_axis = 6378137. ;
		grid_mapping:inverse_flattening = 298.257223563 ;
		grid_mapping:longitude_of_prime_meridian = 0. ;
	int part_node_count(part) ;
		part_node_count:long_name = "count of nodes in each geometry part" ;
	int interior_ring(part) ;
		interior_ring:long_name = "type of each geometry part" ;

// global attributes:
		:Conventions = "CF-1.8" ;
data:

 x_nodes = 35, 45, 15, 10, 35, 20, 35, 30, 20 ;

 y_nodes = 10, 45, 40, 20, 10, 30, 35, 20, 30 ;

 geometry_container = _ ;

 node_count = 9 ;

 grid_mapping = _ ;

 part_node_count = 5, 4 ;

 interior_ring = 0, 1 ;
}
  
```  

## MultiPolygon with One Interior Ring (2D)  
Well Known Text (WKT): ```MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)), ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20))) ```  
Common Data Language (CDL):
```  
netcdf sample_multipolygon_hole {
dimensions:
	node = 14 ;
	instance = 1 ;
	part = 3 ;
variables:
	double x_nodes(node) ;
		x_nodes:units = "degrees_east" ;
		x_nodes:standard_name = "longitude" ;
		x_nodes:axis = "X" ;
	double y_nodes(node) ;
		y_nodes:units = "degrees_north" ;
		y_nodes:standard_name = "latitude" ;
		y_nodes:axis = "Y" ;
	float geometry_container ;
		geometry_container:geometry_type = "polygon" ;
		geometry_container:node_count = "node_count" ;
		geometry_container:node_coordinates = "x_nodes y_nodes" ;
		geometry_container:grid_mapping = "grid_mapping" ;
		geometry_container:part_node_count = "part_node_count" ;
		geometry_container:interior_ring = "interior_ring" ;
	int node_count(instance) ;
		node_count:long_name = "count of coordinates in each instance geometry" ;
	float grid_mapping ;
		grid_mapping:grid_mapping_name = "latitude_longitude" ;
		grid_mapping:semi_major_axis = 6378137. ;
		grid_mapping:inverse_flattening = 298.257223563 ;
		grid_mapping:longitude_of_prime_meridian = 0. ;
	int part_node_count(part) ;
		part_node_count:long_name = "count of nodes in each geometry part" ;
	int interior_ring(part) ;
		interior_ring:long_name = "type of each geometry part" ;

// global attributes:
		:Conventions = "CF-1.8" ;
data:

 x_nodes = 40, 20, 45, 40, 20, 10, 10, 30, 45, 20, 30, 20, 20, 30 ;

 y_nodes = 40, 45, 30, 40, 35, 30, 10, 5, 20, 35, 20, 15, 25, 20 ;

 geometry_container = _ ;

 node_count = 14 ;

 grid_mapping = _ ;

 part_node_count = 4, 6, 4 ;

 interior_ring = 0, 0, 1 ;
}
  
```  

## Multiple MultiPolygons with Interior Rings (2D)  
Well Known Text (WKT): ```MULTIPOLYGON(((0 0, 20 0, 20 20, 0 20, 0 0), (1 1, 10 5, 19 1, 1 1), (5 15, 7 19, 9 15, 5 15), (11 15, 13 19, 15 15, 11 15)), ((5 25, 9 25, 7 29, 5 25)), ((11 25, 15 25, 13 29, 11 25))) ```  
Common Data Language (CDL):
```  
netcdf sample_multipolygons_holes {
dimensions:
	node = 25 ;
	instance = 1 ;
	part = 6 ;
variables:
	double x_nodes(node) ;
		x_nodes:units = "degrees_east" ;
		x_nodes:standard_name = "longitude" ;
		x_nodes:axis = "X" ;
	double y_nodes(node) ;
		y_nodes:units = "degrees_north" ;
		y_nodes:standard_name = "latitude" ;
		y_nodes:axis = "Y" ;
	float geometry_container ;
		geometry_container:geometry_type = "polygon" ;
		geometry_container:node_count = "node_count" ;
		geometry_container:node_coordinates = "x_nodes y_nodes" ;
		geometry_container:grid_mapping = "grid_mapping" ;
		geometry_container:part_node_count = "part_node_count" ;
		geometry_container:interior_ring = "interior_ring" ;
	int node_count(instance) ;
		node_count:long_name = "count of coordinates in each instance geometry" ;
	float grid_mapping ;
		grid_mapping:grid_mapping_name = "latitude_longitude" ;
		grid_mapping:semi_major_axis = 6378137. ;
		grid_mapping:inverse_flattening = 298.257223563 ;
		grid_mapping:longitude_of_prime_meridian = 0. ;
	int part_node_count(part) ;
		part_node_count:long_name = "count of nodes in each geometry part" ;
	int interior_ring(part) ;
		interior_ring:long_name = "type of each geometry part" ;

// global attributes:
		:Conventions = "CF-1.8" ;
data:

 x_nodes = 0, 20, 20, 0, 0, 1, 10, 19, 1, 5, 7, 9, 5, 11, 13, 15, 11, 5, 9, 
    7, 5, 11, 15, 13, 11 ;

 y_nodes = 0, 0, 20, 20, 0, 1, 5, 1, 1, 15, 19, 15, 15, 15, 19, 15, 15, 25, 
    25, 29, 25, 25, 25, 29, 25 ;

 geometry_container = _ ;

 node_count = 25 ;

 grid_mapping = _ ;

 part_node_count = 5, 4, 4, 4, 4, 4 ;

 interior_ring = 0, 1, 1, 1, 0, 0 ;
}
  
```  

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            -Error : Can't find DESCRIPTION
|-| -|-|-|-|══ Building pkgdown site ════════════════════════════════════════════════════════════════════
-Reading from: '/Users/dblodgett/Documents/Projects/WBEEP/3_code/ncdfgeom'
|Writing to:   '/Users/dblodgett/Documents/Projects/WBEEP/3_code/ncdfgeom/docs/dev'
-── Initialising site ────────────────────────────────────────────────────────────────────────
|Copying '../../../../../../../Library/Frameworks/R.framework/Versions/3.5/Resources/library/pkgdown/assets/pkgdown.css' to 'pkgdown.css'
Copying '../../../../../../../Library/Frameworks/R.framework/Versions/3.5/Resources/library/pkgdown/assets/pkgdown.js' to 'pkgdown.js'
-|── Building home ────────────────────────────────────────────────────────────────────────────
-Writing 'authors.html'
|-|-|-|Writing 'index.html'
-── Building function reference ──────────────────────────────────────────────────────────────
|-Updating ncdfgeom documentation
|Writing NAMESPACE
-Loading ncdfgeom
|-|This information is preliminary or provisional 
and is subject to revision. It is being provided 
to meet the need for timely best science. The 
information has not received final approval by the 
U.S. Geological Survey (USGS) and is provided on the 
condition that neither the USGS nor the U.S. Government 
shall be held liable for any damages resulting from the 
authorized or unauthorized use of the information.

****Support Package****
This package is a USGS-R Support package. 
see: https://owi.usgs.gov/R/packages.html#support
-Writing NAMESPACE
|Writing 'reference/index.html'
-Loading ncdfgeom
|-Reading 'man/read_attribute_data.Rd'
This information is preliminary or provisional 
and is subject to revision. It is being provided 
to meet the need for timely best science. The 
information has not received final approval by the 
U.S. Geological Survey (USGS) and is provided on the 
condition that neither the USGS nor the U.S. Government 
shall be held liable for any damages resulting from the 
authorized or unauthorized use of the information.

****Support Package****
This package is a USGS-R Support package. 
see: https://owi.usgs.gov/R/packages.html#support
|-|-Writing 'reference/read_attribute_data.html'
|Reading 'man/read_geometry.Rd'
-|-Writing 'reference/read_geometry.html'
|Reading 'man/read_timeseries_dsg.Rd'
-Writing 'reference/read_timeseries_dsg.html'
|Reading 'man/write_attribute_data.Rd'
-Writing 'reference/write_attribute_data.html'
|Reading 'man/write_geometry.Rd'
-Writing 'reference/write_geometry.html'
|Reading 'man/write_point_dsg.Rd'
-Writing 'reference/write_point_dsg.html'
|Reading 'man/write_timeseries_dsg.Rd'
-Writing 'reference/write_timeseries_dsg.html'
|── Building articles ────────────────────────────────────────────────────────────────────────
-Writing 'articles/index.html'
|Reading 'vignettes/geometry.Rmd'
-|-|-|-|-|-|-|Writing 'articles/geometry.html'
-Reading 'vignettes/ncdfgeom.Rmd'
|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-Error in download.file(url = prcp_url, destfile = prcp_file, quiet = TRUE) : 
  cannot open URL 'ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpndv-v1.0.0-20180806'
|-|- -|-|-|-══ Building pkgdown site ════════════════════════════════════════════════════════════════════
|Reading from: '/Users/dblodgett/Documents/Projects/WBEEP/3_code/ncdfgeom'
-Writing to:   '/Users/dblodgett/Documents/Projects/WBEEP/3_code/ncdfgeom/docs/dev'
|── Initialising site ────────────────────────────────────────────────────────────────────────
-── Building home ────────────────────────────────────────────────────────────────────────────
|-|-|-|Writing 'index.html'
-── Building function reference ──────────────────────────────────────────────────────────────
|-Updating ncdfgeom documentation
|Writing NAMESPACE
-Loading ncdfgeom
|-|This information is preliminary or provisional 
and is subject to revision. It is being provided 
to meet the need for timely best science. The 
information has not received final approval by the 
U.S. Geological Survey (USGS) and is provided on the 
condition that neither the USGS nor the U.S. Government 
shall be held liable for any damages resulting from the 
authorized or unauthorized use of the information.

****Support Package****
This package is a USGS-R Support package. 
see: https://owi.usgs.gov/R/packages.html#support
-Writing NAMESPACE
|Loading ncdfgeom
-|This information is preliminary or provisional 
and is subject to revision. It is being provided 
to meet the need for timely best science. The 
information has not received final approval by the 
U.S. Geological Survey (USGS) and is provided on the 
condition that neither the USGS nor the U.S. Government 
shall be held liable for any damages resulting from the 
authorized or unauthorized use of the information.

****Support Package****
This package is a USGS-R Support package. 
see: https://owi.usgs.gov/R/packages.html#support
-── Building articles ────────────────────────────────────────────────────────────────────────
|Reading 'vignettes/ncdfgeom.Rmd'
-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|Writing 'articles/ncdfgeom.html'
-|══ DONE ═════════════════════════════════════════════════════════════════════════════════════
-|-|-| ── Previewing site ──────────────────────────────────────────────────────────────────────────
