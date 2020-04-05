
#' Create and validate well-known text
#'
#' Like other geo types, [geo_wkt()] doesn't convert its input
#' but does validate it using [validate_geo_wkt()].
#' To skip validation, use [new_geo_wkt()].
#'
#' @param x A character vector containing well-known text
#'
#' @return A [new_geo_wkt()]
#' @export
#'
#' @examples
#' geo_wkt("POINT (30 10)")
#'
geo_wkt <- function(x = character()) {
  x <- vec_cast(x, character())
  wkt <- validate_geo_wkt(new_geo_wkt(x))
  wkt
}


#' S3 details for geo_wkt
#'
#' @inheritParams geo_wkt
#' @param ... Unused
#' @param y,to A prototype to cast to. See [vctrs::vec_cast()] and
#'   [vctrs::vec_ptype2()]
#'
#' @export
#'
#' @examples
#' wkt <- geo_wkt("POINT (30 10)")
#' is_geo_wkt(wkt)
#'
new_geo_wkt <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = c("geo_wkt", "geovctr"))
}

#' @rdname new_geo_wkt
#' @export
is_geo_wkt <- function(x) {
  inherits(x, "geo_wkt")
}

#' @rdname new_geo_wkt
#' @export
validate_geo_wkt <- function(x) {
  is_parseable <- cpp_validate_provider(x)
  stop_for_non_parseable(is_parseable)
  invisible(x)
}

#' @export
vec_ptype_abbr.geo_wkt <- function(x, ...) {
  "wkt"
}

#' @export
format.geo_wkt <- function(x, ..., trunc_width = 40, col = FALSE) {
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
      maybe_green(substr(abbreved, geom_type_start, geom_type_end), col = col),
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

#' @rdname new_geo_wkt
#' @export
as_geo_wkt <- function(x, ...) {
  UseMethod("as_geo_wkt")
}

#' @rdname new_geo_wkt
#' @export
as_geo_wkt.default <- function(x, ...) {
  vec_cast(x, geo_wkt())
}

#' @method vec_cast geo_wkt
#' @export
#' @export vec_cast.geo_wkt
#' @rdname new_geo_wkt
vec_cast.geo_wkt <- function(x, to, ...) {
  UseMethod("vec_cast.geo_wkt")
}

#' @method vec_cast.geo_wkt default
#' @export
vec_cast.geo_wkt.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_wkt geo_wkt
#' @export
vec_cast.geo_wkt.geo_wkt <- function(x, to, ...) {
  x
}

#' @method vec_cast.geo_wkt character
#' @export
vec_cast.geo_wkt.character <- function(x, to, ...) {
  geo_wkt(x)
}

#' @method vec_cast.character geo_wkt
#' @export
vec_cast.character.geo_wkt <- function(x, to, ...) {
  vec_data(x)
}

#' @method vec_cast.geo_wkt geo_wkb
#' @export
vec_cast.geo_wkt.geo_wkb <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_wkt geo_rect
#' @export
vec_cast.geo_wkt.geo_rect <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_wkt geo_segment
#' @export
vec_cast.geo_wkt.geo_segment <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_wkt geo_xy
#' @export
vec_cast.geo_wkt.geo_xy <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_wkt geo_collection
#' @export
vec_cast.geo_wkt.geo_collection <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_ptype2 geo_wkt
#' @export
#' @export vec_ptype2.geo_wkt
#' @rdname new_geo_wkt
vec_ptype2.geo_wkt <- function(x, y, ...) {
  UseMethod("vec_ptype2.geo_wkt", y)
}

#' @method vec_ptype2.geo_wkt default
#' @export
vec_ptype2.geo_wkt.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.geo_wkt geo_wkt
#' @export
vec_ptype2.geo_wkt.geo_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geo_wkt geo_wkb
#' @export
vec_ptype2.geo_wkt.geo_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geo_wkt geo_collection
#' @export
vec_ptype2.geo_wkt.geo_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geo_wkt geo_xy
#' @export
vec_ptype2.geo_wkt.geo_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geo_wkt geo_segment
#' @export
vec_ptype2.geo_wkt.geo_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geo_wkt geo_rect
#' @export
vec_ptype2.geo_wkt.geo_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}
