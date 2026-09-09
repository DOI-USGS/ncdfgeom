# Write attribute data to NetCDF-CF

Creates a NetCDF file with an instance dimension, and any attributes
from a data frame. Use to create the start of a NetCDF-DSG file. One
character length dimension is created long enough to contain the longest
provided character string. This function does not implement any CF
convention attributes or standard names. Any columns of class date will
be converted to character.

## Usage

``` r
write_attribute_data(
  nc_file,
  att_data,
  instance_dim_name = "instance",
  units = rep("unknown", ncol(att_data)),
  overwrite = FALSE
)
```

## Arguments

- nc_file:

  `character` file path to the nc file to be created. If adding to a
  file, it must already have the named instance dimension.

- att_data:

  `data.frame` with instances as columns and attributes as rows.

- instance_dim_name:

  `character` name for the instance dimension. Defaults to "instance"

- units:

  `character` vector with units for each column of att_data. Defaults to
  "unknown" for all.

- overwrite:

  boolean overwrite existing file? Will append if FALSE.

## Examples

``` r
sample_data <- sf::st_set_geometry(sf::read_sf(system.file("shape/nc.shp", 
                                                           package = "sf")), 
                                   NULL)
example_file <-write_attribute_data(tempfile(), sample_data,
                                    units = rep("unknown", ncol(sample_data)))

try({
  ncdump <- system(paste("ncdump -h", example_file), intern = TRUE)
  cat(ncdump ,sep = "\n")
}, silent = TRUE)
```
