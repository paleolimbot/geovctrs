
#' Create and validate well-known text
#'
#' Like other geo types, [geo_wkt()] doesn't convert its input
#' but does validate it using [wk::validate_wk_wkt()].
#' To skip validation, use [wk::new_wk_wkt()].
#'
#' @param x A character vector containing well-known text
#' @param trim Trim unnecessary zeroes in the output?
#' @param precision The rounding precision to use when writing
#'   (number of decimal places).
#' @param dimensions The maximum number of dimensions to consider
#'   when generating text.
#' @param ... Unused
#'
#'
#' @return A [wk::new_wk_wkt()]
#' @export
#'
#' @examples
#' # use geo_wkt() to "mark" a vector as well-known text
#' geo_wkt("POINT (30 10)")
#'
#' # use as_geo_wkt() to use conversion options
#' as_geo_wkt("POINT (30 10)", trim = FALSE, precision = 2)
#'
#' # use parse_wkt() to identify parsing failures
#' parse_wkt("POINT EMTPY")
#'
geo_wkt <- function(x = character()) {
  wkt(x)
}

#' @export
#' @rdname geo_wkt
parse_wkt <- function(x) {
  x <- vec_cast(x, character())
  validate_provider(wk::new_wk_wkt(x), wk::wkt_problems(x))
}

#' @rdname geo_wkt
#' @export
as_geo_wkt <- function(x, ..., trim = TRUE, precision = 16, dimensions = 3) {
  UseMethod("as_geo_wkt")
}

#' @export
as_geo_wkt.default <- function(x, ..., trim = TRUE, precision = 16, dimensions = 3) {
  vec_cast(x, new_wk_wkt())
}

#' S3 details for wk_wkt
#'
#' @inheritParams geo_wkt
#' @param y,to A prototype to cast to. See [vctrs::vec_cast()] and
#'   [vctrs::vec_ptype2()]
#'
#' @export
#'
#' @examples
#' wkt <- geo_wkt("POINT (30 10)")
#' is_wk_wkt(wkt)
#'
is_wk_wkt <- function(x) {
  inherits(x, "wk_wkt")
}

#' @export
`[<-.wk_wkt` <- function(x, i, value) {
  x <- unclass(x)
  x[i] <- as_wkt(value)
  new_wk_wkt(x)
}

# shim
#' @export
as_wkt.geovctrs_wkb <- function(x, ...) {
  new_wk_wkt(wk::wkb_translate_wkt(x))
}

#' @method vec_cast wk_wkt
#' @export
#' @export vec_cast.wk_wkt
#' @rdname is_wk_wkt
vec_cast.wk_wkt <- function(x, to, ...) {
  UseMethod("vec_cast.wk_wkt")
}

#' @method vec_cast.wk_wkt default
#' @export
vec_cast.wk_wkt.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.wk_wkt wk_wkt
#' @export
vec_cast.wk_wkt.wk_wkt <- function(x, to, ...) {
  x
}

#' @method vec_cast.wk_wkt character
#' @export
vec_cast.wk_wkt.character <- function(x, to, ...) {
  geovctrs_cpp_convert(geo_wkt(x), to)
}

#' @method vec_cast.character wk_wkt
#' @export
vec_cast.character.wk_wkt <- function(x, to, ...) {
  vec_data(x)
}

#' @method vec_cast.wk_wkt geovctrs_wkb
#' @export
vec_cast.wk_wkt.geovctrs_wkb <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkt geovctrs_rect
#' @export
vec_cast.wk_wkt.geovctrs_rect <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkt geovctrs_segment
#' @export
vec_cast.wk_wkt.geovctrs_segment <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkt geovctrs_xy
#' @export
vec_cast.wk_wkt.geovctrs_xy <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkt geovctrs_xyz
#' @export
vec_cast.wk_wkt.geovctrs_xyz <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkt geovctrs_collection
#' @export
vec_cast.wk_wkt.geovctrs_collection <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_ptype2 wk_wkt
#' @export
#' @export vec_ptype2.wk_wkt
#' @rdname is_wk_wkt
vec_ptype2.wk_wkt <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_wkt", y)
}

#' @method vec_ptype2.wk_wkt default
#' @export
vec_ptype2.wk_wkt.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.wk_wkt wk_wkt
#' @export
vec_ptype2.wk_wkt.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  # always use default conversion args when combining to avoid loosing data
  geo_wkt()
}

#' @method vec_ptype2.wk_wkt geovctrs_wkb
#' @export
vec_ptype2.wk_wkt.geovctrs_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.wk_wkt geovctrs_collection
#' @export
vec_ptype2.wk_wkt.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.wk_wkt geovctrs_xy
#' @export
vec_ptype2.wk_wkt.geovctrs_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.wk_wkt geovctrs_xyz
#' @export
vec_ptype2.wk_wkt.geovctrs_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.wk_wkt geovctrs_segment
#' @export
vec_ptype2.wk_wkt.geovctrs_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.wk_wkt geovctrs_rect
#' @export
vec_ptype2.wk_wkt.geovctrs_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}
