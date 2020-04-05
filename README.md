
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geovctrs

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![R-CMD-check](https://github.com/paleolimbot/geovctrs/workflows/R-CMD-check/badge.svg)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/geovctrs/branch/master/graph/badge.svg)](https://codecov.io/gh/paleolimbot/geovctrs?branch=master)
<!-- badges: end -->

The goal of geovctrs is to provide a common set of classes and data
structures to ensure that processing functions in the rapidly expanding
R geospatial ecosystem are interchangable.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/geovctrs")
```

If you can load the package, you’re good to go\!

``` r
library(geovctrs)
```

## Example

This package has reasonable `plot()` and `print()` methods for WKB and
WKT types, and provides a [vctrs](https://vctrs.r-lib.org/)
implementation for these types so that they “just work” with
[tibble](https://tibble.tidyverse.org/),
[tidyr](https://tidyr.tidyverse.org/), and
[dplyr](https://dplyr.tidyverse.org/) (\>=1.0.0) (among others).

``` r
head(geo_example_wkt)
#> <geo_wkt[6]>
#> [1] NA_wkt_                                
#> [2] POINT (30 10)                          
#> [3] POINT EMPTY                            
#> [4] POINT Z (1 1 5)                        
#> [5] MULTIPOINT (10 40, 40 30, 20 20, 30 10)
#> [6] MULTIPOINT EMPTY
head(as_geo_wkb(geo_example_wkt))
#> <geo_wkb[6]>
#> [1] NA_wkb_                  POINT (30 10)           
#> [3] POINT EMPTY              POINT (1 1)             
#> [5] MULTIPOINT[4] (10 40)…+3 MULTIPOINT EMPTY
```

Most methods work on anything that can be interpreted as geometry,
including character vectors and data.frames containing exactly one
geometry column.

``` r
geo_plot("LINESTRING (30 10, 10 30, 40 40)")
```

<img src="man/figures/README-ex-plot-1.png" width="100%" />

Like any self-respecting geometry package, geovctrs contains a copy of
the North Carolina dataset to play with:

``` r
rev(geo_nc)
#> # A tibble: 100 x 12
#>    geometry                       NWBIR79 SID79 BIR79 NWBIR74 SID74 BIR74
#>    <wkb>                            <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>
#>  1 △▽[1] (-81.47276 36.23436)…+26      19     0  1364      10     1  1091
#>  2 △▽[1] (-81.23989 36.36536)…+25      12     3   542      10     0   487
#>  3 △▽[1] (-80.45634 36.24256)…+27     260     6  3616     208     5  3188
#>  4 △▽[3] (-76.00897 36.31960)…+37     145     2   830     123     1   508
#>  5 △▽[1] (-77.21767 36.24098)…+33    1197     3  1606    1066     9  1421
#>  6 △▽[1] (-76.74506 36.23392)…+21    1237     5  1838     954     7  1452
#>  7 △▽[1] (-76.00897 36.31960)…+23     139     2   350     115     0   286
#>  8 △▽[1] (-76.56251 36.34057)…+16     371     2   594     254     0   420
#>  9 △▽[1] (-78.30876 36.26004)…+13     844     2  1190     748     4   968
#> 10 △▽[1] (-80.02567 36.25023)…+5      176     5  2038     160     1  1612
#> # … with 90 more rows, and 5 more variables: CRESS_ID <int>, FIPSNO <dbl>,
#> #   FIPS <chr>, NAME <chr>, CNTY_ID <dbl>
```
