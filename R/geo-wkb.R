
#' Create and validate well-known binary
#'
#' Like other geo types, [geo_wkb()] doesn't convert its input
#' but does validate it using [validate_geo_wkb()].
#' To skip validation, use [new_geo_wkb()] with
#' the result of `vec_cast(list(...), .ptype = raw())`.
#'
#' @param x A [list()] of [raw()] objects, each of which
#'   represent well-known binary
#' @param include_srid Use `TRUE` to always include, `FALSE`
#'   to never include, or `NA` to only include if the SRID
#'   is non-zero.
#' @param endian Use `0` for big endian, `1` for little endian
#'   or `NA` to use the default on your system.
#' @inheritParams geo_wkt
#'
#' @return A [new_geo_wkb()]
#' @export
#'
#' @examples
#' # POINT (30 10) in WKB
#' wkb_item <- as.raw(
#'   c(
#'     0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
#'     0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
#'     0x00, 0x00, 0x00, 0x24, 0x40
#'   )
#' )
#' wkb <- geo_wkb(list(wkb_item))
#' wkb
#'
#' # use as_geo_wkb() to use conversion options
#' as_geo_wkb(wkb, endian = 0)
#'
geo_wkb <- function(x = list()) {
  x <- vec_cast(x,  list_of(.ptype = raw()))
  wkb <- validate_geo_wkb(new_geo_wkb(x))
  wkb
}

#' @export
#' @rdname geo_wkb
as_geo_wkb <- function(x, ..., include_srid = NA, dimensions = 3, endian = NA) {
  UseMethod("as_geo_wkb")
}

#' @export
#' @rdname new_geo_wkb
as_geo_wkb.default <- function(x, ..., include_srid = NA, dimensions = 3, endian = NA) {
  include_srid <- vec_cast(include_srid, logical())
  dimensions <- vec_cast(dimensions, integer())
  endian <- vec_cast(endian, integer())
  vec_cast(x, new_geo_wkb(include_srid = include_srid, dimensions = dimensions, endian = endian))
}

#' S3 details for geo_wkb
#'
#' @inheritParams geo_wkb
#' @inheritParams new_geo_wkt
#'
#' @export
#'
#' @examples
#' wkb_raw <- as.raw(
#'   c(
#'     0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
#'     0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
#'     0x00, 0x00, 0x00, 0x24, 0x40
#'   )
#' )
#' wkb <- geo_wkb(list(wkb_raw))
#' is_geo_wkb(wkb)
#'
#' # to skip parse validation if you know your object is
#' # valid, use new_geo_wkb()
#' new_geo_wkb(vctrs::new_list_of(list(wkb_raw), raw()))
#'
new_geo_wkb <- function(x = vctrs::list_of(.ptype = raw()),
                        include_srid = NA, dimensions = 3L, endian = NA_integer_) {
  vec_assert(x, list_of(.ptype = raw()))
  new_list_of(
    x,
    raw(),
    class = c("geo_wkb", "geovctr"),
    include_srid = include_srid,
    dimensions = dimensions,
    endian = endian
  )
}

#' @rdname new_geo_wkb
#' @export
is_geo_wkb <- function(x) {
  inherits(x, "geo_wkb")
}

#' @rdname new_geo_wkb
#' @export
validate_geo_wkb <- function(x) {
  is_parseable <- cpp_validate_provider(x)
  stop_for_non_parseable(is_parseable)
  invisible(x)
}

#' @export
vec_ptype_abbr.geo_wkb <- function(x, ...) {
  "wkb"
}

#' @export
format.geo_wkb <- function(x, ...) {
  geo_format(x, ...)
}

#' @export
print.geo_wkb <- function(x, ...) {
  geo_print(x, ...)
}

#' @method vec_cast geo_wkb
#' @export
#' @export vec_cast.geo_wkb
#' @rdname new_geo_wkb
vec_cast.geo_wkb <- function(x, to, ...) {
  UseMethod("vec_cast.geo_wkb")
}

#' @method vec_cast.geo_wkb default
#' @export
vec_cast.geo_wkb.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_wkb geo_wkb
#' @export
vec_cast.geo_wkb.geo_wkb <- function(x, to, ...) {
  if (identical(attributes(x), attributes(to))) {
    x
  } else {
    cpp_convert(x, to)
  }
}

#' @method vec_cast.geo_wkb list
#' @export
vec_cast.geo_wkb.list <- function(x, to, ...) {
  cpp_convert(geo_wkb(x), to)
}

#' @method vec_cast.geo_wkb geo_wkt
#' @export
vec_cast.geo_wkb.geo_wkt <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_wkb geo_rect
#' @export
vec_cast.geo_wkb.geo_rect <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_wkb geo_segment
#' @export
vec_cast.geo_wkb.geo_segment <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_wkb geo_xy
#' @export
vec_cast.geo_wkb.geo_xy <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_wkb geo_collection
#' @export
vec_cast.geo_wkb.geo_collection <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_ptype2 geo_wkb
#' @export
#' @export vec_ptype2.geo_wkb
#' @rdname new_geo_wkb
vec_ptype2.geo_wkb <- function(x, y, ...) {
  UseMethod("vec_ptype2.geo_wkb", y)
}

#' @method vec_ptype2.geo_wkb default
#' @export
vec_ptype2.geo_wkb.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.geo_wkb geo_wkb
#' @export
vec_ptype2.geo_wkb.geo_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geo_wkb geo_wkt
#' @export
vec_ptype2.geo_wkb.geo_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geo_wkb geo_collection
#' @export
vec_ptype2.geo_wkb.geo_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geo_wkb geo_xy
#' @export
vec_ptype2.geo_wkb.geo_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geo_wkb geo_segment
#' @export
vec_ptype2.geo_wkb.geo_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geo_wkb geo_rect
#' @export
vec_ptype2.geo_wkb.geo_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}
