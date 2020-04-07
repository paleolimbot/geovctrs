
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
#> [3] POINT EMPTY              POINT Z (1 1)           
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

## sf and data frame support

The geovctrs package has tentative sf and data frame support for all
processing functions, following the principle that well-mannered
transformation functions return the same type and shape as the input.
For data frames with exactly one geovctr column, transformation
functions replace the output column with another geovctr (this might be
of a different type, but will be a geovctr).

``` r
geo_envelope(rev(geo_nc))
#> # A tibble: 100 x 12
#>    geometry                  NWBIR79 SID79 BIR79 NWBIR74 SID74 BIR74
#>    <rect>                      <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>
#>  1 △ (-81.74107 36.23436)…+4      19     0  1364      10     1  1091
#>  2 △ (-81.34754 36.36536)…+4      12     3   542      10     0   487
#>  3 △ (-80.96577 36.23388)…+4     260     6  3616     208     5  3188
#>  4 △ (-76.33025 36.07282)…+4     145     2   830     123     1   508
#>  5 △ (-77.90121 36.16277)…+4    1197     3  1606    1066     9  1421
#>  6 △ (-77.21767 36.23024)…+4    1237     5  1838     954     7  1452
#>  7 △ (-76.56358 36.16973)…+4     139     2   350     115     0   286
#>  8 △ (-76.95367 36.29452)…+4     371     2   594     254     0   420
#>  9 △ (-78.32125 36.19595)…+4     844     2  1190     748     4   968
#> 10 △ (-80.45301 36.25023)…+4     176     5  2038     160     1  1612
#> # … with 90 more rows, and 5 more variables: CRESS_ID <int>, FIPSNO <dbl>,
#> #   FIPS <chr>, NAME <chr>, CNTY_ID <dbl>
```

For sf objects, this means that valid sf objects are returned:

``` r
library(sf)
#> Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0
sf_nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
geo_envelope(sf_nc)
#> Simple feature collection with 100 features and 14 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> epsg (SRID):    4267
#> proj4string:    +proj=longlat +datum=NAD27 +no_defs
#> # A tibble: 100 x 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74
#>    <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1
#> # … with 90 more rows, and 5 more variables: NWBIR74 <dbl>, BIR79 <dbl>,
#> #   SID79 <dbl>, NWBIR79 <dbl>, geometry <POLYGON [°]>
```

Other functions that accept geometry as input will treat sf and data
frame objects as vectors with the length of `nrow(x)`:

``` r
geo_n_coordinates(sf_nc)
#>   [1] 27 26 28 38 34 22 24 17 14  6  7  7 17  6 13 53 20 47 23 12 17 28 24
#>  [24] 20 22  6 10 45  9 18 23 34 28 27 33 37 27 30 19 29 17 31 34 18 16 35
#>  [47]  7 27 19 18 45 14 45 27 39 24 36 44 23 24 33 29 22 21 11 36 29 31 14
#>  [70] 24 32 35 27 31 22 19 16 29 40 20 27 29 31 18 27 14 31 30 36 17 39 17
#>  [93] 19 25 30 31 34 33 23 27
geo_plot(sf_nc)
```

<img src="man/figures/README-sf-nc-plot-1.png" width="100%" />
