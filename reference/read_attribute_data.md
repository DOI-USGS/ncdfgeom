# Read attribute dataframe from NetCDF-DSG file

Gets attribute data from a NetCDF-DSG file and returns it in a
`data.frame`. This function is intended as a convenience to be used
within workflows where the netCDF file is already open and well
understood.

## Usage

``` r
read_attribute_data(nc, instance_dim)
```

## Arguments

- nc:

  A NetCDF path or url to be opened.

- instance_dim:

  The NetCDF instance/station dimension.

## Examples

``` r
hucPolygons <- sf::read_sf(system.file('extdata','example_huc_eta.json', package = 'ncdfgeom'))
hucPolygons_nc <- ncdfgeom::write_geometry(tempfile(), hucPolygons)

read_attribute_data(hucPolygons_nc, "instance")
#>   id                                  TNMID
#> 1  1 {94745C74-B81F-42C7-9397-09DE87E038D4}
#> 2  2 {D756B24D-1959-4938-8930-FC3D417BB708}
#>                               METASOURCE SOURCEDATA SOURCEORIG SOURCEFEAT
#> 1 {2A42A390-4EEB-411B-928E-188529D9691D}                                 
#> 2                                                                        
#>     LOADDATE GNIS_ID AREAACRES  AREASQKM STATES        HUC12         NAME
#> 1 2013-01-18             28635 115.88012     NC 030101030106    Big Creek
#> 2 2013-01-18              8464  34.25262     NC 030101030107 Double Creek
#>   HUTYPE HUMOD        TOHUC NONCONTRIB NONCONTR_1 SHAPE_Leng  SHAPE_Area
#> 1      S    NM 030101030109          0          0  0.6684346 0.011654300
#> 2      S    NM 030101030109          0          0  0.3739526 0.003442696
#>   node_count
#> 1       1678
#> 2        950
```
