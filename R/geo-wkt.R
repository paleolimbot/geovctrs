
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
  validate_provider(new_geovctrs_wkt(x))
}

#' @rdname geo_wkt
#' @export
as_geo_wkt <- function(x, ..., trim = TRUE, precision = 16, dimensions = 3) {
  UseMethod("as_geo_wkt")
}

#' @export
as_geo_wkt.default <- function(x, ..., trim = TRUE, precision = 16, dimensions = 3) {
  trim <- vec_cast(trim, logical())
  precision <- vec_cast(precision, integer())
  dimensions <- vec_cast(dimensions, integer())
  vec_cast(x, new_geovctrs_wkt(trim = trim, precision = precision, dimensions = dimensions))
}

#' S3 details for geo_wkt
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
new_geovctrs_wkt <- function(x = character(), trim = TRUE, precision = 16L, dimensions = 3L) {
  vec_assert(x, character())
  vec_assert(trim, logical())
  vec_assert(precision, integer())
  vec_assert(dimensions, integer())
  new_vctr(
    x,
    class = c("geovctrs_wkt", "geovctr"),
    trim = trim,
    precision = precision,
    dimensions = dimensions
  )
}

#' @rdname new_geovctrs_wkt
#' @export
is_geovctrs_wkt <- function(x) {
  inherits(x, "geovctrs_wkt")
}

#' @rdname new_geovctrs_wkt
#' @export
validate_geovctrs_wkt <- function(x) {
  stop_for_non_parseable(validate_provider(x))
  invisible(x)
}

#' @export
vec_ptype_abbr.geovctrs_wkt <- function(x, ...) {
  "wkt"
}

#' @export
format.geovctrs_wkt <- function(x, ..., trunc_width = 40, col = FALSE) {
  # collapse whitespace, remove leading whitespace
  x <- gsub("\\s+", " ", gsub("^\\s*", "", gsub("\\s*$", "", x)))
  trunc <- substr(x, 1, trunc_width - 1)
  width <- nchar(x)

  abbreved <- ifelse(
    width > (trunc_width - 1),
    paste0(trunc, "\U2026"),
    x
  )

  geom_type_match <- regexpr("[A-Z ]+", abbreved)
  geom_type_start <- as.integer(geom_type_match)
  geom_type_end <- geom_type_start + attr(geom_type_match, "match.length") - 1

  formatted <- ifelse(
    geom_type_match != -1,
    paste0(
      maybe_grey(substr(abbreved, geom_type_start, geom_type_end), col = col),
      maybe_blue(substr(abbreved, geom_type_end + 1, trunc_width), col = col)
    ),
    maybe_blue(abbreved, col = col)
  )

  ifelse(
    is.na(x),
    maybe_red("NA_wkt_", col = col),
    formatted
  )
}

#' @export
obj_print_data.geovctrs_wkt <- function(x, ...) {
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
    cpp_convert(x, to)
  }
}

#' @method vec_cast.geovctrs_wkt character
#' @export
vec_cast.geovctrs_wkt.character <- function(x, to, ...) {
  # by default, normalize the input (user can skirt this by calling
  # geo_wkt() instead of as_geo_wkt()
  cpp_convert(geo_wkt(x), to)
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
  cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_rect
#' @export
vec_cast.geovctrs_wkt.geovctrs_rect <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_segment
#' @export
vec_cast.geovctrs_wkt.geovctrs_segment <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_xy
#' @export
vec_cast.geovctrs_wkt.geovctrs_xy <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_wkt geovctrs_collection
#' @export
vec_cast.geovctrs_wkt.geovctrs_collection <- function(x, to, ...) {
  cpp_convert(x, to)
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
