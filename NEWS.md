1.2.3
==========
* switched the polygon intersection vignette from nhdplusTools to hydrogeofetch
* read_timeseries_dsg() now falls back to a standard_name of station_id when no
  variable carries a cf_role of timeseries_id
* corrected the normalize = TRUE documentation of
  calculate_area_intersection_weights() (#102)
* minimum R version is now 4.1, matching sf, stars and dplyr

1.2.2
==========
* migrated test and example spatial data from shapefile to GeoPackage (#107)
* improved CRS handling in write_geometry() with clearer warnings when a CRS is missing or cannot be mapped to a CF grid mapping
* internal code cleanup

1.2.1
==========
* improved documentation of calculate_area_intersection_weights()

1.2.0
==========
* calculate_area_intersection_weights() now supports normalized weights

1.1.6
==========
* remove geoknife as suggested package
* reduce installed package size

1.1.5
==========
* update URLs for new repository.

ncdfgeom 1.1.1
==========
* Updates to read and write timeseries DSG. Should be backwards compatible.

ncdfgeom 1.1.0
==========
* Added class `ncdfgeom` to response from `read_timeseries_dsg` for compatibility with `stars`.
* Added citation information.
* Bug fix in handling of multilinestrings.