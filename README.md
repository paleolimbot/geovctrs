
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geovctrs

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![R-CMD-check](https://github.com/paleolimbot/geovctrs/workflows/R-CMD-check/badge.svg)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/geovctrs/branch/master/graph/badge.svg)](https://codecov.io/gh/paleolimbot/geovctrs?branch=master)
<!-- badges: end -->

The goal of geovctrs is to provide [low-level access to the GEOS
library](https://geos.osgeo.org/doxygen/geos__c_8h_source.html),
supporting several common input/output formats to facilitate
geoprocessing in R. This package tries to solve the “hard” problems
associated with wrapping a C/C++ library, exposing an R API and a C++
API that can be used in dependency packages.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/geovctrs")
```

If you can load the package, you’re good to go\!

``` r
library(geovctrs)
```

## Example

Create a line, buffer it, and plot\!

``` r
line <- geo_wkt("LINESTRING (30 10, 10 30, 40 40)")
geo_plot(geos_buffer(line, width = 4), col = "grey90")
geo_plot_add(line)
```

<img src="man/figures/README-ex-plot-1.png" width="100%" />

## Philosophy

  - Minimize conversion between in-memory formats
  - Operate one feature at a time
  - Use [vctrs](https://vctrs.r-lib.org/) where possible to make sure
    that geometry vectors can exist in a data frame.
  - Clear size and type stability
  - Don’t consider spatial constraints (That’s what
    [sf](https://r-spatial.github.io/sf) is so good at\!)
