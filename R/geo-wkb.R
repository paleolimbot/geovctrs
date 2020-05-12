
#' Create and validate well-known binary
#'
#' Like other geo types, [geo_wkb()] doesn't convert its input
#' but does validate it using [validate_geovctrs_wkb()].
#' To skip validation, use [new_geovctrs_wkb()] with
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
#' @return A [new_geovctrs_wkb()]
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
  x <- as.list(x)
  wkb <- validate_geovctrs_wkb(new_geovctrs_wkb(x))
  wkb
}

#' @export
#' @rdname geo_wkb
parse_wkb <- function(x) {
  validate_provider(new_geovctrs_wkb(x), wk::wkb_problems(x))
}

#' @export
#' @rdname geo_wkb
as_geo_wkb <- function(x, ..., include_srid = NA, dimensions = 3, endian = NA) {
  UseMethod("as_geo_wkb")
}

#' @export
as_geo_wkb.default <- function(x, ..., include_srid = NA, dimensions = 3, endian = NA) {
  vec_cast(x, new_geovctrs_wkb())
}

#' S3 details for geovctrs_wkb
#'
#' @inheritParams geo_wkb
#' @inheritParams new_geovctrs_wkt
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
#' is_geovctrs_wkb(wkb)
#'
#' # to skip parse validation if you know your object is
#' # valid, use new_geovctrs_wkb()
#' new_geovctrs_wkb(list(wkb_raw))
#'
new_geovctrs_wkb <- function(x = list()) {
  vec_assert(x, list())
  new_vctr(x, class = c("geovctrs_wkb", "geovctr"))
}

#' @rdname new_geovctrs_wkb
#' @export
is_geovctrs_wkb <- function(x) {
  inherits(x, "geovctrs_wkb")
}

#' @rdname new_geovctrs_wkb
#' @export
validate_geovctrs_wkb <- function(x) {
  stop_for_non_parseable(validate_provider(x, wk::wkb_problems(x)))
  invisible(x)
}

#' @export
vec_ptype_abbr.geovctrs_wkb <- function(x, ...) {
  "wkb"
}

#' @export
format.geovctrs_wkb <- function(x, ..., trunc_width = 40, max_coords = 5, col = FALSE) {
  formatted_wkt <- format.geovctrs_wkt(
    wk::wkb_format(x, max_coords = max_coords),
    trunc_width = trunc_width,
    col = col
  )

  paste0("<", formatted_wkt, ">")
}

#' @export
obj_print_data.geovctrs_wkb <- function(x, ...) {
  if (length(x) == 0) {
    return()
  }

  print_default_colour(
    format(x, col = FALSE),
    format(x, col = TRUE),
    ...
  )

  invisible(x)
}

#' @method vec_cast geovctrs_wkb
#' @export
#' @export vec_cast.geovctrs_wkb
#' @rdname new_geovctrs_wkb
vec_cast.geovctrs_wkb <- function(x, to, ...) {
  UseMethod("vec_cast.geovctrs_wkb")
}

#' @method vec_cast.geovctrs_wkb default
#' @export
vec_cast.geovctrs_wkb.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geovctrs_wkb geovctrs_wkb
#' @export
vec_cast.geovctrs_wkb.geovctrs_wkb <- function(x, to, ...) {
  if (identical(attributes(x), attributes(to))) {
    x
  } else {
    geovctrs_cpp_convert(x, to)
  }
}

#' @method vec_cast.geovctrs_wkb character
#' @export
vec_cast.geovctrs_wkb.character <- function(x, to, ...) {
  geovctrs_cpp_convert(geo_wkt(x), to)
}

#' @method vec_cast.geovctrs_wkb list
#' @export
vec_cast.geovctrs_wkb.list <- function(x, to, ...) {
  geovctrs_cpp_convert(geo_wkb(x), to)
}

#' @method vec_cast.geovctrs_wkb geovctrs_wkt
#' @export
vec_cast.geovctrs_wkb.geovctrs_wkt <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkb geovctrs_rect
#' @export
vec_cast.geovctrs_wkb.geovctrs_rect <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkb geovctrs_segment
#' @export
vec_cast.geovctrs_wkb.geovctrs_segment <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkb geovctrs_xy
#' @export
vec_cast.geovctrs_wkb.geovctrs_xy <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkb geovctrs_xyz
#' @export
vec_cast.geovctrs_wkb.geovctrs_xyz <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkb geovctrs_collection
#' @export
vec_cast.geovctrs_wkb.geovctrs_collection <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_ptype2 geovctrs_wkb
#' @export
#' @export vec_ptype2.geovctrs_wkb
#' @rdname new_geovctrs_wkb
vec_ptype2.geovctrs_wkb <- function(x, y, ...) {
  UseMethod("vec_ptype2.geovctrs_wkb", y)
}

#' @method vec_ptype2.geovctrs_wkb default
#' @export
vec_ptype2.geovctrs_wkb.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geovctrs_wkb geovctrs_wkb
#' @export
vec_ptype2.geovctrs_wkb.geovctrs_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geovctrs_wkb geovctrs_wkt
#' @export
vec_ptype2.geovctrs_wkb.geovctrs_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geovctrs_wkb geovctrs_collection
#' @export
vec_ptype2.geovctrs_wkb.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geovctrs_wkb geovctrs_xy
#' @export
vec_ptype2.geovctrs_wkb.geovctrs_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geovctrs_wkb geovctrs_xyz
#' @export
vec_ptype2.geovctrs_wkb.geovctrs_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geovctrs_wkb geovctrs_segment
#' @export
vec_ptype2.geovctrs_wkb.geovctrs_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geovctrs_wkb geovctrs_rect
#' @export
vec_ptype2.geovctrs_wkb.geovctrs_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}
