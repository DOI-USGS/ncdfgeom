# Write geometries and attributes to NetCDF-CF

Creates a file with point, line or polygon instance data ready for the
extended NetCDF-CF timeSeries featuretype format.

Will also add attributes if provided data has them.

## Usage

``` r
write_geometry(
  nc_file,
  geom_data,
  instance_dim_name = NULL,
  variables = list()
)
```

## Arguments

- nc_file:

  `character` file path to the nc file to be created.

- geom_data:

  sf `data.frame` with POINT, LINESTRING, MULTILINESTRING, POLYGON, or
  MULTIPOLYGON geometries. Note that three dimensional geometries are
  not supported.

- instance_dim_name:

  `character` Not required if adding geometry to a NetCDF-CF Discrete
  Sampling Geometries timeSeries file. For a new file, will use package
  default – "instance" – if not supplied.

- variables:

  `character` If a an existing netCDF files is provided, this list of
  variables that should be related to the geometries.

## References

1.  <https://cfconventions.org/cf-conventions/cf-conventions.html>

## Examples

``` r

hucPolygons <- sf::read_sf(system.file('extdata','example_huc_eta.json', package = 'ncdfgeom'))

hucPolygons_nc <- ncdfgeom::write_geometry(nc_file=tempfile(), 
                                           geom_data = hucPolygons)
try({
  ncdump <- system(paste("ncdump -h", hucPolygons_nc), intern = TRUE)
  cat(ncdump ,sep = "\n")
}, silent = TRUE)
```
