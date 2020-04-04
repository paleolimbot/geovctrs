
#' Missing geometries, missing coordinates and non-finite geometries
#'
#' Whereas EMPTY geometries are a concept at the geometry level,
#' missing values are a concept of the in-memory vectors used to store
#' them in R. The [geo_is_missing()] function can be used to identify
#' these values, similar to [is.na()]. Missing coordinates (`NA` or `NaN`)
#' can be identified using [geo_has_missing()], and [geo_is_finite()]
#' can be used to ensure that all coordinates are finite.
#'
#' @inheritParams geo_bbox
#'
#' @return A logical vector
#' @export
#'
#' @examples
#' geo_is_missing(geo_wkt()[NA_integer_])
#'
#' geo_is_missing(geo_wkt("LINESTRING (10 inf, nan 2)"))
#' geo_has_missing(geo_wkt("LINESTRING (10 inf, nan 2)"))
#' geo_is_finite(geo_wkt("LINESTRING (10 inf, nan 2)"))
#'
#' geo_is_missing(geo_wkt("LINESTRING (10 inf, 1 2)"))
#' geo_has_missing(geo_wkt("LINESTRING (10 inf, 1 2)"))
#' geo_is_finite(geo_wkt("LINESTRING (10 inf, 1 2)"))
#'
#' # EMPTY geometries are considered finite and non-missing
#' geo_is_missing(geo_wkt("LINESTRING EMPTY"))
#' geo_has_missing(geo_wkt("LINESTRING EMPTY"))
#' geo_is_finite(geo_wkt("LINESTRING EMPTY"))
#'
#' # POINT EMPTY and POINT (nan nan) are tricky corner
#' # cases that currently behave unpredictably depending on
#' # the storage type
#' geo_is_missing(geo_wkt("POINT EMPTY"))
#' geo_has_missing(geo_wkt("POINT EMPTY"))
#' geo_is_finite(geo_wkt("POINT EMPTY"))
#'
#' geo_is_missing(geo_wkt("POINT (nan nan)"))
#' geo_has_missing(geo_wkt("POINT (nan nan)"))
#' geo_is_finite(geo_wkt("POINT (nan nan)"))
#'
#' geo_is_missing(geo_xy(NA, NA))
#' geo_has_missing(geo_xy(NA, NA))
#' geo_is_finite(geo_xy(NA, NA))
#'
geo_is_missing <- function(x) {
  UseMethod("geo_is_missing")
}

#' @export
geo_is_missing.default <- function(x) {
  is.na(as_geovctr(x))
}

#' @rdname geo_is_missing
#' @export
geo_has_missing <- function(x) {
  UseMethod("geo_has_missing")
}

#' @export
geo_has_missing.default <- function(x) {
  geo_has_missing(as_geovctr(x))
}

#' @export
geo_has_missing.geovctr <- function(x) {
  cpp_has_missing(x)
}

#' @export
geo_has_missing.geo_wkt <- function(x) {
  grepl("nan", vec_data(x))
}

#' @export
geo_has_missing.geo_xy <- function(x) {
  is.na(field(x, "x")) | is.na(field(x, "y"))
}

#' @export
geo_has_missing.geo_segment <- function(x) {
  start <- field(x, "start")
  end <- field(x, "end")
  is.na(field(start, "x")) |
    is.na(field(start, "y")) |
    is.na(field(end, "x")) |
    is.na(field(end, "y"))
}

#' @export
geo_has_missing.geo_rect <- function(x) {
  is.na(field(x, "xmin")) |
    is.na(field(x, "ymin")) |
    is.na(field(x, "xmax")) |
    is.na(field(x, "ymax"))
}

#' @rdname geo_is_missing
#' @export
geo_is_finite <- function(x) {
  UseMethod("geo_is_finite")
}

#' @export
geo_is_finite.default <- function(x) {
  geo_is_finite(as_geovctr(x))
}

#' @export
geo_is_finite.geovctr <- function(x) {
  cpp_is_finite(x)
}

#' @export
geo_is_finite.geo_wkt <- function(x) {
  !grepl("inf|nan", vec_data(x))
}

#' @export
geo_is_finite.geo_xy <- function(x) {
  is.na(x) |
    is.finite(field(x, "x")) &
    is.finite(field(x, "y"))
}

#' @export
geo_is_finite.geo_segment <- function(x) {
  start <- field(x, "start")
  end <- field(x, "end")
  is.finite(field(start, "x")) &
    is.finite(field(start, "y")) &
    is.finite(field(end, "x")) &
    is.finite(field(end, "y"))
}

#' @export
geo_is_finite.geo_rect <- function(x) {
  is.finite(field(x, "xmin")) &
    is.finite(field(x, "ymin")) &
    is.finite(field(x, "xmax")) &
    is.finite(field(x, "ymax"))
}
