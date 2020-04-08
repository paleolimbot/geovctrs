
#' Test for geovctrs
#'
#' A geovctr is a geometry-like collection of objects. In the geovctrs
#' package, [geo_wkt()], [geo_wkb()], [geo_collection()],
#' [geo_xy()], [geo_rect()], and [geo_segment()] are all geovctrs.
#' Extension packages can either use these types or
#' implement the [as_geovctr()] generic
#' to take advantage of a wide range of processing functions, including
#' coercion, plotting, and summary information.
#'
#' This package is intended to allow for a variety of in-memory
#' representations of geometry, as there are many examples where
#' simple geometries can be efficiently parameterized without
#' resorting to storing every coordinate of every vertex
#' (built-in examples include [geo_xy()], [geo_segment()], and
#' [geo_rect()]). These types do, however, have unambiguous
#' representations as geometries, and thus should be able to be
#' used wherever a geometry is appropriate.
#'
#' For an object to be a "geovctr", it must:
#'
#' - Be a vctr ([vctrs::vec_is()] must be `TRUE`). This ensures that it will
#'   work with [tibble::tibble()] and other tidyverse functions such as
#'   [tidyr::unnest()] and [dplyr::group_by()] / [dplyr::mutate()] /
#'   [dplyr::summarise()].
#'
#' - Inherit from `"geovctr"`. This makes it work automatically with
#'   functions like [geo_plot()] and [geo_bbox()] that have a default
#'   implementation for something that can be coerced to [geo_wkt()],
#'   [geo_wkb()], or [geo_collection()].
#'
#' - Have the ability to be casted to [geo_wkt()], [geo_wkb()], and [geo_collection()]
#'   using [vec_cast()]. These casts power the default implementations of
#'   functions like [geo_plot()] and [geo_bbox()], and allow geometries to
#'   be combined using [vctrs::vec_c()] (which powers row-binding in
#'   tidyverse functions). This means implementing the appropriate
#'   [vctrs::vec_cast()] methods for a class.
#'
#' - Have the ability to be combined with [geo_wkt()], [geo_wkb()], and [geo_collection()]
#'   using [vec_c()] in both directions. This helps support processing functions
#'   that return a class to be combined with the output of other functions.
#'   This might require a [vctrs::vec_ptype()] implementation for
#'   a class.
#'
#' You can test these expectations for a given object using [expect_geovctr()].
#'
#' A secondary class of object is one that *could*  be interpreted as a geovctr,
#' but in most cases can't be. One example of this is a character vector,
#' which *could* be well-known text, but probably isn't. However, when the
#' user passes it to a function like [geo_plot()] or [geo_bbox()], it
#' probably *is* well-known text. Similarly, a `data.frame` or
#' [tibble::tibble()] probably doesn't contain a geometry column,
#' but when passed to a function that operates on geometries,
#' it's likely that it does. The geovctrs package supports these
#' objects with the [as_geovctr()] generic, which means you can
#' pass these objects anywhere you would pass a first-class
#' geometry vector.
#'
#' @param x A (possibly) geovctr
#' @param result The result of a transformation operation
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
#' as_geovctr(tibble::tibble(geometry = geo_wkt("POINT (30 10)")))
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
as_geovctr.data.frame <- function(x, ...) {
  x[[find_geovctr_column(x)]]
}

#' @rdname is_geovctr
#' @export
as_geovctr.sfc <- function(x, ...) {
  wkb_list <- unclass(sf_compat_as_binary(x, ..., EWKB = TRUE))
  new_geovctrs_wkb(wkb_list)
}

#' @rdname is_geovctr
#' @export
as_geovctr.sf <- function(x, ...) {
  as_geovctr(x[[attr(x, "sf_column")]], ...)
}

#' @rdname is_geovctr
#' @export
restore_geovctr <- function(x, result, ...) {
  UseMethod("restore_geovctr")
}

#' @rdname is_geovctr
#' @export
restore_geovctr.default <- function(x, result, ...) {
  result
}

#' @rdname is_geovctr
#' @export
restore_geovctr.data.frame <- function(x, result, ...) {
  x[[find_geovctr_column(x)]] <- result
  x
}

#' @rdname is_geovctr
#' @export
restore_geovctr.sfc <- function(x, result, ...) {
  wkb <- as_geo_wkb(result)
  wkb[is.na(wkb)] <- as_geo_wkb("GEOMETRYCOLLECTION EMPTY")
  class(wkb) <- "WKB"
  sf_compat_as_sfc(wkb, ..., EWKB = TRUE)
}

#' @rdname is_geovctr
#' @export
restore_geovctr.sf <- function(x, result, ...) {
  x[[attr(x, "sf_column")]] <- restore_geovctr(x[[attr(x, "sf_column")]], result, ...)
  x
}

find_geovctr_column <- function(x) {
  col_is_geovctr <- vapply(x, is_geovctr, logical(1))
  cols <- names(x)[col_is_geovctr]

  if (sum(col_is_geovctr) == 0) {
    abort("Can't find geovctr column in data.frame")
  } else if (sum(col_is_geovctr) > 1) {
    col_labs <- paste0('"', cols, '"', collapse = ", ")
    abort(sprintf("More than one geovctr column in data.frame:\n`%s`", col_labs))
  }

  cols
}

#' @rdname is_geovctr
#' @export
expect_geovctr <- function(x) {
  # must be a vctr and a geovctr
  testthat::expect_true(vec_is(x))
  testthat::expect_true(is_geovctr(x))

  # must be castable to WKT, WKB, and collection
  testthat::expect_is(vec_cast(x, geo_wkb()), "geovctrs_wkb")
  testthat::expect_is(vec_cast(x, geo_wkt()), "geovctrs_wkt")
  testthat::expect_is(vec_cast(x, geo_collection()), "geovctrs_collection")

  # must have implementations for as_WKT, WKB, and collection
  testthat::expect_is(as_geo_wkb(x), "geovctrs_wkb")
  testthat::expect_is(as_geo_wkt(x), "geovctrs_wkt")
  testthat::expect_is(as_geo_collection(x), "geovctrs_collection")

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
