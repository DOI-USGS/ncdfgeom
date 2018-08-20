NetCDF-CF Geometry and Timeseries Tools for R
===
[![Build Status](https://travis-ci.org/USGS-R/ncdfgeom.svg)](https://travis-ci.org/USGS-R/ncdfgeom) [![Coverage Status](https://coveralls.io/repos/github/USGS-R/ncdfgeom/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/ncdfgeom?branch=master)

`ncdfgeom` is an **in development** package that reads and writes geometry data (points lines and polygons), attributes of geometries, and time series associated with the geometries in a standards-compliant way. It implements the NetCDF-CF Spatial Geometries specification and the timeSeries feature type of the [Discrete Sampling Geometry](http://cfconventions.org/cf-conventions/cf-conventions.html#discrete-sampling-geometries) NetCDF-CF specification. 

**Visit the [`pkgdown` site](https://USGS-R.github.io/ncdfgeom/dev/articles/ncdfgeom.html) for a complete overview of the package.**

Given that this package is in active development, please test it out and consider [submitting issues and/or contributions!](https://github.com/USGS-R/ncdfgeom/issues)

## Installation

For the latest version:
```
install.packages("devtools")
devtools::install_github("USGS-R/ncdfgeom")
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
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."
