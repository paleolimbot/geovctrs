
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
parse_wkb <- function(x) {
  validate_provider(new_wk_wkb(x), wk::wkb_problems(x))
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

#' S3 details for wk_wkb
#'
#' @inheritParams geo_wkb
#' @inheritParams is_wk_wkt
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
#' is_wk_wkb(wkb)
#'
#' # to skip parse validation if you know your object is
#' # valid, use new_wk_wkb()
#' new_wk_wkb(list(wkb_raw))
#'
is_wk_wkb <- function(x) {
  inherits(x, "wk_wkb")
}

#' @export
`[<-.wk_wkb` <- function(x, i, value) {
  x <- unclass(x)
  x[i] <- as_wkb(value)
  new_wk_wkb(x)
}

#' @rdname is_wk_wkb
#' @export
validate_wk_wkb <- function(x) {
  stop_for_non_parseable(validate_provider(x, wk::wkb_problems(x)))
  invisible(x)
}

#' @export
vec_ptype_abbr.wk_wkb <- function(x, ...) {
  "wkb"
}

#' @export
vec_proxy.wk_wkb <- function(x, ...) {
  unclass(x)
}

#' @export
vec_restore.wk_wkb <- function(x, ...) {
  wk::new_wk_wkb(x)
}

#' @method vec_cast wk_wkb
#' @export
#' @export vec_cast.wk_wkb
#' @rdname is_wk_wkb
vec_cast.wk_wkb <- function(x, to, ...) {
  UseMethod("vec_cast.wk_wkb")
}

#' @method vec_cast.wk_wkb default
#' @export
vec_cast.wk_wkb.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.wk_wkb wk_wkb
#' @export
vec_cast.wk_wkb.wk_wkb <- function(x, to, ...) {
  if (identical(attributes(x), attributes(to))) {
    x
  } else {
    geovctrs_cpp_convert(x, to)
  }
}

#' @method vec_cast.wk_wkb character
#' @export
vec_cast.wk_wkb.character <- function(x, to, ...) {
  geovctrs_cpp_convert(geo_wkt(x), to)
}

#' @method vec_cast.wk_wkb list
#' @export
vec_cast.wk_wkb.list <- function(x, to, ...) {
  geovctrs_cpp_convert(geo_wkb(x), to)
}

#' @method vec_cast.wk_wkb wk_wkt
#' @export
vec_cast.wk_wkb.wk_wkt <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

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

#' @method vec_ptype2 wk_wkb
#' @export
#' @export vec_ptype2.wk_wkb
#' @rdname is_wk_wkb
vec_ptype2.wk_wkb <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_wkb", y)
}

#' @method vec_ptype2.wk_wkb default
#' @export
vec_ptype2.wk_wkb.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.wk_wkb wk_wkb
#' @export
vec_ptype2.wk_wkb.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.wk_wkb wk_wkt
#' @export
vec_ptype2.wk_wkb.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

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
