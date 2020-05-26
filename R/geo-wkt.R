
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
  validate_provider(new_wk_wkt(x), wk::wkt_problems(x))
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

#' @importFrom wk validate_wk_wkt
#' @importFrom wk vec_cast.wk_wkt
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

#' @importFrom wk vec_ptype2.wk_wkt
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
