
#' Create and validate well-known binary
#'
#' Like other geo types, [geo_wkb()] doesn't convert its input
#' but does validate it using [validate_wk_wkb()].
#' To skip validation, use [new_wk_wkb()] with
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
#' @return A [new_wk_wkb()]
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
#' # use parse_wkb() to identify parse errors
#' wkb_item[2] <- as.raw(0x11)
#' parse_wkb(list(wkb_item))
#'
geo_wkb <- function(x = list()) {
  wkb(x)
}

#' @export
#' @rdname geo_wkb
as_geo_wkb <- function(x, ..., include_srid = NA, dimensions = 3, endian = NA) {
  UseMethod("as_geo_wkb")
}

#' @export
as_geo_wkb.default <- function(x, ..., include_srid = NA, dimensions = 3, endian = NA) {
  vec_cast(x, new_wk_wkb())
}

#' @importFrom wk validate_wk_wkb
#' @importFrom wk vec_cast.wk_wkb
#' @method vec_cast.wk_wkb geovctrs_rect
#' @export
vec_cast.wk_wkb.geovctrs_rect <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkb geovctrs_segment
#' @export
vec_cast.wk_wkb.geovctrs_segment <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkb geovctrs_xy
#' @export
vec_cast.wk_wkb.geovctrs_xy <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkb geovctrs_xyz
#' @export
vec_cast.wk_wkb.geovctrs_xyz <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkb geovctrs_collection
#' @export
vec_cast.wk_wkb.geovctrs_collection <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @importFrom wk vec_ptype2.wk_wkb
#' @method vec_ptype2.wk_wkb geovctrs_collection
#' @export
vec_ptype2.wk_wkb.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.wk_wkb geovctrs_xy
#' @export
vec_ptype2.wk_wkb.geovctrs_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.wk_wkb geovctrs_xyz
#' @export
vec_ptype2.wk_wkb.geovctrs_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.wk_wkb geovctrs_segment
#' @export
vec_ptype2.wk_wkb.geovctrs_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.wk_wkb geovctrs_rect
#' @export
vec_ptype2.wk_wkb.geovctrs_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}
