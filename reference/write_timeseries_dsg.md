# Write time series to NetCDF-CF

This function creates a timeseries discrete sampling geometry NetCDF
file. It uses the orthogonal array encoding to write one `data.frame`
per function call. This encoding is best suited to data with the same
number of timesteps per instance (e.g. geometry or station).

## Usage

``` r
write_timeseries_dsg(
  nc_file,
  instance_names,
  lats,
  lons,
  times,
  data,
  alts = NA,
  data_unit = "",
  data_prec = "double",
  data_metadata = list(name = "data", long_name = "unnamed data"),
  time_units = "days since 1970-01-01 00:00:00",
  instance_dim_name = "instance",
  dsg_timeseries_id = "instance_name",
  coordvar_long_names = list(instance = "Station Names", time = "time of measurement",
    lat = "latitude of the measurement", lon = "longitude of the measurement", alt =
    "altitude of the measurement"),
  attributes = list(),
  add_to_existing = FALSE,
  overwrite = FALSE
)
```

## Arguments

- nc_file:

  `character` file path to the nc file to be created.

- instance_names:

  `character` or `numeric` vector of names for each instance (e.g.
  station or geometry) to be added to the file.

- lats:

  `numeric` vector of latitudes

- lons:

  `numeric` vector of longitudes

- times:

  `POSIXct` vector of times. Must be of type `POSIXct` or an attempt to
  convert it will be made using `as.POSIXct(times)`.

- data:

  `data.frame` with each column corresponding to an instance. Rows
  correspond to time steps. nrow must be the same length as times.
  Column names must match instance names.

- alts:

  `numeric` vector of altitudes (m above sea level) (Optional)

- data_unit:

  `character` vector of data units. Length must be the same as number of
  columns in `data` parameter.

- data_prec:

  `character` precision of observation data in NetCDF file. Valid
  options: 'short' 'integer' 'float' 'double' 'char'.

- data_metadata:

  `list` A named list of strings: list(name='ShortVarName', long_name='A
  Long Name')

- time_units:

  `character` units string in udunits format to use for time. Defaults
  to 'days since 1970-01-01 00:00:00'

- instance_dim_name:

  the `character` name to use for the instance used in
  \`instance_names\`

- dsg_timeseries_id:

  the `character` name to use for the instance used in the timeseries id

- coordvar_long_names:

  `list` values for long names on coordinate variables. Names should be
  \`instance\`, time\`, \`lat\`, \`lon\`, and \`alt.\`

- attributes:

  list An optional list of attributes that will be added at the global
  level. See details for useful attributes.

- add_to_existing:

  `boolean` If TRUE and the file already exists, variables will be added
  to the existing file. See details for more.

- overwrite:

  boolean unless set to true, error if file exists.

## Details

Suggested Global Variables: c(title = "title", abstract = "history",
provider site = "institution", provider name ="source", description =
"description")

Note regarding add_to_existing: add_to_existing = TRUE should only be
used to add variables to an existing NetCDF discrete sampling geometry
file. All other inputs should be the same as are already in the file. If
the functions is called with add_to_existing=FALSE (the default), it
will overwrite an existing file with the same name. The expected usage
is to call this function repeatedly only changing the data, data_unit,
data_prec and data_metadata inputs.

See the timeseries vignette for more information.

## References

1.  <https://docs.unidata.ucar.edu/netcdf-java/4.6/userguide/reference/FeatureDatasets/CFpointImplement.html>

2.  <https://cfconventions.org/cf-conventions/cf-conventions.html#_orthogonal_multidimensional_array_representation>

3.  <https://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/cf-conventions.html#time-series-data>
