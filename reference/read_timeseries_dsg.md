# Read NetCDF-CF timeSeries featuretype

This function reads a timeseries discrete sampling geometry NetCDF file
and returns a list containing the file's contents.

## Usage

``` r
read_timeseries_dsg(nc_file, read_data = TRUE)
```

## Arguments

- nc_file:

  character file path to the nc file to be read.

- read_data:

  logical whether to read metadata only or not.

## Value

list containing the contents of the NetCDF file.

## Details

The current implementation checks several NetCDF-CF specific conventions
prior to attempting to read the file. The Conventions and featureType
global attributes are checked but not strictly required.

Variables with standard_name and/or cf_role of station_id and/or
timeseries_id are searched for to indicate which variable is the
'timeseries identifier'. The function stops if one is not found.

All variables are introspected for a coordinates attribute. This
attribute is used to determine which variables are coordinate variables.
If none are found an attempt to infer data variables by time and
timeseries_id dimensions is made.

The coordinates variables are introspected and their standard_names used
to determine which coordinate they are. Lat, lon, and time are required,
height is not.

Variables with a coordinates attribute are assumed to be the 'data
variables'.

Data variables are traversed and their metadata and data content put
into lists within the main response list.

See the timeseries vignette for more information.

## References

https://docs.unidata.ucar.edu/netcdf-java/4.6/userguide/reference/FeatureDatasets/CFpointImplement.html
