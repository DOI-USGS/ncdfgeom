# Reading and Writing Timeseries

First, we’ll load up some time series data.

``` r

attribute_file<-system.file('extdata/yahara_alb_attributes.csv', package = "ncdfgeom")

attributes <- read.csv(attribute_file, colClasses='character')
lats <- as.numeric(attributes$YCOORD)
lons <- as.numeric(attributes$XCOORD)
alts <- rep(1,length(lats)) # Making up altitude for the sake of demonstration.
```

We now have vectors of latitudes, longitudes, altitudes for each of our
time series.

``` r


# can use geoknife from github
# timeseries_file <- system.file('extdata/yahara_alb_gdp_file.csv', package = "ncdfgeom")
# raw_data <- geoknife::parseTimeseries(timeseries_file, delim=',', with.units=TRUE)

raw_data <- readRDS(system.file('extdata/yahara_alb_gdp_file.rds', package = "ncdfgeom"))

timeseries_data <- raw_data[2:(ncol(raw_data) - 3)]

time <- raw_data$DateTime

long_name <- paste(raw_data$variable[1], 'area weighted', raw_data$statistic[1], 'in', 
                   raw_data$units[1], sep=' ')

meta <- list(name=raw_data$variable[1], long_name=long_name)
```

Now we have the `timeseries_data` data.frame of timeseries data, the
`time` vector of timesteps, and a bit of metadata for the timeseries
variable that we will write into the NetCDF file.

``` r

nc_summary<-'example summary'
nc_date_create<-'2099-01-01'
nc_creator_name='example creator'
nc_creator_email='example@test.com'
nc_project='example ncdfgeom'
nc_proc_level='just an example no processing'
nc_title<-'example title'

global_attributes<-list(title = nc_title, 
                        summary = nc_summary, 
                        date_created = nc_date_create, 
                        creator_name = nc_creator_name,
                        creator_email = nc_creator_email, 
                        project = nc_project,
                        processing_level = nc_proc_level)

ncdfgeom::write_timeseries_dsg(nc_file = "demo_nc.nc", 
                               instance_names = names(timeseries_data),
                               lats = lats, 
                               lons = lons, 
                               alts = alts,
                               times = time, 
                               data = timeseries_data,
                               data_unit = raw_data$units[1],
                               data_prec = 'double',
                               data_metadata = meta,
                               attributes = global_attributes) -> nc_file
```

Now we have a NetCDF file with reference spatial information for each
time series, and a single timeseries variable.

The file has three dimensions.

1.  A dimension called “instance” that is the same length as the number
    of timeseries written,
2.  one called “time” that is the length of the number of time series,
    and
3.  one called “name_strlen” that is the length of the longest
    timeseries ID (which is stored as a string).

``` r

ncmeta::nc_dims(nc_file)
#> # A tibble: 3 × 4
#>      id name               length unlim
#>   <int> <chr>               <dbl> <lgl>
#> 1     0 instance               71 FALSE
#> 2     1 time                  730 FALSE
#> 3     2 instance_name_char      2 FALSE
```

The file has variables for latitude, longitude, altitude, timeseries
IDs, and a data variable.

``` r

ncmeta::nc_vars(nc_file)
#> # A tibble: 6 × 5
#>      id name                                        type      ndims natts
#>   <int> <chr>                                       <chr>     <int> <int>
#> 1     0 instance_name                               NC_CHAR       2     2
#> 2     1 time                                        NC_DOUBLE     1     4
#> 3     2 lat                                         NC_DOUBLE     1     4
#> 4     3 lon                                         NC_DOUBLE     1     4
#> 5     4 alt                                         NC_DOUBLE     1     4
#> 6     5 BCCA_0-125deg_pr_day_ACCESS1-0_rcp45_r1i1p1 NC_DOUBLE     2     4
```

The primary dimensions in the file are of length, number of time steps
and number of time series.

``` r

ncmeta::nc_dims(nc_file)
#> # A tibble: 3 × 4
#>      id name               length unlim
#>   <int> <chr>               <dbl> <lgl>
#> 1     0 instance               71 FALSE
#> 2     1 time                  730 FALSE
#> 3     2 instance_name_char      2 FALSE
```

The header of the resulting NetCDF file looks like:

This file can be read back into R with the function
`read_timeseries_dsg`. The response is a list of variables as shown
below.

``` r

timeseries_dataset <- ncdfgeom::read_timeseries_dsg(nc_file)
names(timeseries_dataset)
#> [1] "time"              "lats"              "lons"             
#> [4] "alts"              "varmeta"           "data_unit"        
#> [7] "data_prec"         "data_frames"       "global_attributes"
```

1.  `time`, `lats`, `lons`, and `alts` are vectors that apply to the
    whole dataset.
2.  `varmeta` has one entry per timeseries variable read from the NetCDF
    file and contains the `name` and `long_name` attribute of each
    variable.
3.  `data_unit` and `data_prec` contain units and precision metadata for
    each variable.
4.  `data_frames` is a list containing one `data.frame` for each
    variable read from the NetCDF file.
5.  `global_attributes` contains standard global attributes found in the
    file. All of the variables that have one element per timeseries
    variable, are named the same as the NetCDF variable names so they
    can be accessed by name as shown below.
