NetCDF-CF Geometry and Timeseries Tools for R
===
![R-CMD-check](https://github.com/usgs-r/ncdfgeom/workflows/R-CMD-check/badge.svg) [![codecov](https://codecov.io/gh/usgs-r/ncdfgeom/branch/master/graph/badge.svg?token=5c1L38yK5q)](https://app.codecov.io/gh/usgs-r/ncdfgeom) [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ncdfgeom)](https://cran.r-project.org/package=ncdfgeom) [![CRAN](https://www.r-pkg.org/badges/version/ncdfgeom)](https://cran.r-project.org/package=ncdfgeom)

`ncdfgeom` reads and writes geometry data (points lines and polygons), attributes of geometries, and time series associated with the geometries in a standards-compliant way.

It implements the NetCDF-CF Spatial Geometries specification and the timeSeries feature type of the [Discrete Sampling Geometry](http://cfconventions.org/cf-conventions/cf-conventions.html#discrete-sampling-geometries) NetCDF-CF specification. 

**Visit the [`pkgdown` site](https://usgs-r.github.io/ncdfgeom/articles/ncdfgeom.html) for a complete overview of the package.**

Given that this package is fairly new and in active development, please test it out 
and consider [submitting issues and/or contributions!](https://github.com/USGS-R/ncdfgeom/issues)

## Installation

ncdfgeom is available via CRAN.

```
install.packages("ncdfgeom")
```

For the latest development version:
```
install.packages("remotes")
remotes::install_github("USGS-R/ncdfgeom")
```

## Contributing

First, thanks for considering a contribution! I hope to make this package a community created resource for us all to gain from and won’t be able to do that without your help!

1. Contributions should be thoroughly tested with testthat.
1. Code style should attempt to follow the tidyverse style guide.
1. Please attempt to describe what you want to do prior to contributing by submitting an issue.
1. Please follow the typical github fork - pull-request workflow.
1. Make sure you use roxygen and run Check before contributing. More on this front as the package matures.

## Package Status

[![status](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)

This package is considered a 'support' package. For more information, see:
[https://owi.usgs.gov/R/packages.html#support](https://owi.usgs.gov/R/packages.html#support)

## Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."
