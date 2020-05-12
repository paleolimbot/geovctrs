
#' Create and validate well-known text
#'
#' Like other geo types, [geo_wkt()] doesn't convert its input
#' but does validate it using [validate_geovctrs_wkt()].
#' To skip validation, use [new_geovctrs_wkt()].
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
#' @return A [new_geovctrs_wkt()]
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
  x <- vec_cast(x, character())
  wkt <- validate_geovctrs_wkt(new_geovctrs_wkt(x))
  wkt
}

#' @export
#' @rdname geo_wkt
parse_wkt <- function(x) {
  x <- vec_cast(x, character())
  validate_provider(new_geovctrs_wkt(x), wk::wkt_problems(x))
}

#' @rdname geo_wkt
#' @export
as_geo_wkt <- function(x, ..., trim = TRUE, precision = 16, dimensions = 3) {
  UseMethod("as_geo_wkt")
}

#' @export
as_geo_wkt.default <- function(x, ..., trim = TRUE, precision = 16, dimensions = 3) {
  vec_cast(x, new_geovctrs_wkt())
}

#' S3 details for geovctrs_wkt
#'
#' @inheritParams geo_wkt
#' @param y,to A prototype to cast to. See [vctrs::vec_cast()] and
#'   [vctrs::vec_ptype2()]
#'
#' @export
#'
#' @examples
#' wkt <- geo_wkt("POINT (30 10)")
#' is_geovctrs_wkt(wkt)
#'
#' # use new_geovctrs_wkt() to skip parse validation if you know
#' # your text is valid WKT
#' new_geovctrs_wkt("POINT (30 10)")
#'
new_geovctrs_wkt <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = c("geovctrs_wkt", "geovctr"))
}

#' @rdname new_geovctrs_wkt
#' @export
is_geovctrs_wkt <- function(x) {
  inherits(x, "geovctrs_wkt")
}

#' @rdname new_geovctrs_wkt
#' @export
validate_geovctrs_wkt <- function(x) {
  stop_for_non_parseable(validate_provider(x, wk::wkt_problems(x)))
  invisible(x)
}

#' @export
vec_ptype_abbr.geovctrs_wkt <- function(x, ...) {
  "wkt"
}

#' @export
format.geovctrs_wkt <- function(x, ..., trunc_width = 40, col = FALSE) {
  format_wkt_summary(x, ..., trunc_width = trunc_width, col = col)
}

#' @method vec_cast geovctrs_wkt
#' @export
#' @export vec_cast.geovctrs_wkt
#' @rdname new_geovctrs_wkt
vec_cast.geovctrs_wkt <- function(x, to, ...) {
  UseMethod("vec_cast.geovctrs_wkt")
}

#' @method vec_cast.geovctrs_wkt default
#' @export
vec_cast.geovctrs_wkt.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_wkt
#' @export
vec_cast.geovctrs_wkt.geovctrs_wkt <- function(x, to, ...) {
  if (identical(attributes(x), attributes(to))) {
    x
  } else {
    geovctrs_cpp_convert(x, to)
  }
}

#' @method vec_cast.geovctrs_wkt character
#' @export
vec_cast.geovctrs_wkt.character <- function(x, to, ...) {
  # by default, normalize the input (user can skirt this by calling
  # geo_wkt() instead of as_geo_wkt()
  geovctrs_cpp_convert(geo_wkt(x), to)
}

#' @export
as.character.geovctrs_wkt <- function(x, ...) {
  # override default geovctr method, because this is more
  # intuitive representation
  vec_cast(x, character())
}


#' @method vec_cast.character geovctrs_wkt
#' @export
vec_cast.character.geovctrs_wkt <- function(x, to, ...) {
  vec_data(x)
}

#' @method vec_cast.geovctrs_wkt geovctrs_wkb
#' @export
vec_cast.geovctrs_wkt.geovctrs_wkb <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_rect
#' @export
vec_cast.geovctrs_wkt.geovctrs_rect <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_segment
#' @export
vec_cast.geovctrs_wkt.geovctrs_segment <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_xy
#' @export
vec_cast.geovctrs_wkt.geovctrs_xy <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_xyz
#' @export
vec_cast.geovctrs_wkt.geovctrs_xyz <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_collection
#' @export
vec_cast.geovctrs_wkt.geovctrs_collection <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_ptype2 geovctrs_wkt
#' @export
#' @export vec_ptype2.geovctrs_wkt
#' @rdname new_geovctrs_wkt
vec_ptype2.geovctrs_wkt <- function(x, y, ...) {
  UseMethod("vec_ptype2.geovctrs_wkt", y)
}

#' @method vec_ptype2.geovctrs_wkt default
#' @export
vec_ptype2.geovctrs_wkt.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geovctrs_wkt geovctrs_wkt
#' @export
vec_ptype2.geovctrs_wkt.geovctrs_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  # always use default conversion args when combining to avoid loosing data
  geo_wkt()
}

#' @method vec_ptype2.geovctrs_wkt geovctrs_wkb
#' @export
vec_ptype2.geovctrs_wkt.geovctrs_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geovctrs_wkt geovctrs_collection
#' @export
vec_ptype2.geovctrs_wkt.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geovctrs_wkt geovctrs_xy
#' @export
vec_ptype2.geovctrs_wkt.geovctrs_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geovctrs_wkt geovctrs_xyz
#' @export
vec_ptype2.geovctrs_wkt.geovctrs_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geovctrs_wkt geovctrs_segment
#' @export
vec_ptype2.geovctrs_wkt.geovctrs_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geovctrs_wkt geovctrs_rect
#' @export
vec_ptype2.geovctrs_wkt.geovctrs_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}
