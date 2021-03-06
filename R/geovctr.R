
#' Test for geovctrs
#'
#' A geovctr is a geometry-like collection of objects. In the geovctrs
#' package, [wkt()], [wkb()], [geo_collection()],
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
#' - Have `is_geovctr()` return `TRUE`. This makes it work automatically with
#'   functions like [geo_bbox()] that have a default
#'   implementation for something that can be coerced to [wkt()],
#'   [wkb()], or [wksxp()].
#'
#' - Have the ability to be coerced to [wkt()], [wkb()], and [wksxp()]
#'   using [as_wkt()], [as_wkb()], and [as_wksxp()].
#'   These casts power the default implementations of
#'   functions like [geo_bbox()]. In addition, a [vctrs::vec_cast()]
#'   method should be provided so that row-binding and other vector
#'   operations work with functions that might return a simpler type.
#'
#' - Have the ability to be combined with [wkt()], [wkb()], and [wksxp()]
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
#' user passes it to a function like [geo_bbox()], it
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
#' is_geovctr(wkt())
#' is_geovctr(NULL)
#'
#' as_geovctr(wkt("POINT (30 10)"))
#' as_geovctr("POINT (30 10)")
#' as_geovctr(tibble::tibble(geometry = wkt("POINT (30 10)")))
#'
is_geovctr <- function(x) {
  UseMethod("is_geovctr")
}

#' @export
is_geovctr.default <- function(x) {
  FALSE
}

#' @export
is_geovctr.wk_vctr <- function(x) {
  TRUE
}

#' @export
is_geovctr.geovctrs_xy <- function(x) {
  TRUE
}

#' @export
is_geovctr.geovctrs_xyz <- function(x) {
  TRUE
}

#' @export
is_geovctr.geovctrs_segment <- function(x) {
  TRUE
}

#' @export
is_geovctr.geovctrs_rect <- function(x) {
  TRUE
}

#' @rdname is_geovctr
#' @export
as_geovctr <- function(x, ...) {
  UseMethod("as_geovctr")
}

#' @export
as_geovctr.default <- function(x, ...) {
  if (is_geovctr(x)) {
    x
  } else {
    as_wksxp(x)
  }
}

#' @rdname is_geovctr
#' @export
as_geovctr.character <- function(x, ...) {
  wkt(x, ...)
}

#' @rdname is_geovctr
#' @export
as_geovctr.data.frame <- function(x, ...) {
  x[[find_geovctr_column(x)]]
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
  x <- vec_recycle(x, vec_size(result))
  x[[find_geovctr_column(x)]] <- result
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

  # must be castable to WKT, WKB, and wksxp
  testthat::expect_is(vec_cast(x, wkb()), "wk_wkb")
  testthat::expect_is(vec_cast(x, wkt()), "wk_wkt")
  testthat::expect_is(vec_cast(x, wksxp()), "wk_wksxp")

  # must have implementations for as_WKT, WKB, and wksxp
  testthat::expect_is(as_wkb(x), "wk_wkb")
  testthat::expect_is(as_wkt(x), "wk_wkt")
  testthat::expect_is(as_wksxp(x), "wk_wksxp")

  # must be combinable with wkb, wkt, and wksxp
  testthat::expect_silent(vec_c(wkb(), x))
  testthat::expect_silent(vec_c(wkt(), x))
  testthat::expect_silent(vec_c(wksxp(), x))


  # must be reverse combinable with wkb, wkt, and wksxp
  testthat::expect_silent(vec_c(x, wkb()))
  testthat::expect_silent(vec_c(x, wkt()))
  testthat::expect_silent(vec_c(x, wksxp()))

  invisible(x)
}
