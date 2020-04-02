
#' Test for geovctrs
#'
#' A geovctr is a geometry-like object that can be converted to
#' the standard for geometry, well-known binary. In the geovctrs
#' package, [geo_wkt()], [geo_wkb()], [geo_collection()],
#' [geo_xy()], [geo_rect()], and [geo_segment()] are all geovctrs.
#' Extension packages can implement the [as_geovctr()] generic
#' to take advantage of the functions in this package, including
#' coercion, plotting, and summary information. An example of this
#' is `as_geovctr.character()`, which allow [geo_plot()] to be called
#' on a character vector of well-known text.
#'
#' @param x A (possibly) geovctr
#' @param ... Passed to the constructor
#'
#' @export
#'
#' @examples
#' is_geovctr(geo_wkt())
#' is_geovctr(NULL)
#'
#' as_geovctr(geo_wkt("POINT (30 10)"))
#' as_geovctr("POINT (30 10)")
#'
is_geovctr <- function(x) {
  inherits(x, "geovctr")
}

#' @rdname is_geovctr
#' @export
as_geovctr <- function(x, ...) {
  UseMethod("as_geovctr")
}

#' @rdname is_geovctr
#' @export
as_geovctr.geovctr <- function(x, ...) {
  x
}

#' @rdname is_geovctr
#' @export
as_geovctr.character <- function(x, ...) {
  geo_wkt(x, ...)
}

#' @rdname is_geovctr
#' @export
expect_geovctr <- function(x) {
  # must be a vctr and a geovctr
  testthat::expect_true(vec_is(x))
  testthat::expect_true(is_geovctr(x))

  # must be castable to WKT, WKB, and collection
  testthat::expect_is(vec_cast(x, geo_wkb()), "geo_wkb")
  testthat::expect_is(vec_cast(x, geo_wkt()), "geo_wkt")
  testthat::expect_is(vec_cast(x, geo_collection()), "geo_collection")

  # must have implementations for as_WKT, WKB, and collection
  testthat::expect_is(as_geo_wkb(x), "geo_wkb")
  testthat::expect_is(as_geo_wkt(x), "geo_wkt")
  testthat::expect_is(as_geo_collection(x), "geo_collection")

  # must be combinable with wkb, wkt, and  collection
  testthat::expect_silent(vec_c(geo_wkb(), x))
  testthat::expect_silent(vec_c(geo_wkt(), x))
  testthat::expect_silent(vec_c(geo_collection(), x))

  # must be reverse combinable with wkb, wkt, and  collection
  testthat::expect_silent(vec_c(x, geo_wkb()))
  testthat::expect_silent(vec_c(x, geo_wkt()))
  testthat::expect_silent(vec_c(x, geo_collection()))

  invisible(x)
}
