
#' Line segments
#'
#' The [geo_segment()] type is useful as an efficient representation of
#' line segments stored using column vectors.
#' Note that `geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))` is considered
#' an "empty" linestring, whereas `geo_segment(geo_xy(NA, NA), geo_xy(NA, NA), srid = NA)`
#' is "missing" (see [geo_is_missing()] and [geo_is_empty()]).
#'
#' @param start,end [geo_xy()]s for the start and end
#'   of the segment, respectively.
#' @inheritParams geo_srid
#'
#' @return A [new_geo_segment()]
#' @export
#'
#' @examples
#' geo_plot(geo_segment(geo_xy(0, 0), geo_xy(10, -10:10)))
#'
geo_segment <- function(start = geo_xy(), end = geo_xy(), srid = 0) {
  result <- new_geo_segment(
    vec_recycle_common(
      start = vec_cast(start, geo_xy()),
      end = vec_cast(end, geo_xy()),
      srid = as_geo_srid(srid)
    )
  )

  result
}

#' S3 details for geo_segment
#'
#' @param x A (possibly) [geo_segment()]
#' @inheritParams new_geo_xy
#'
#' @export
#'
new_geo_segment <- function(x = list(start = geo_xy(), end = geo_xy(), srid = integer())) {
  vec_assert(x$start, geo_xy())
  vec_assert(x$end, geo_xy())
  new_rcrd(x, class = c("geo_segment", "geovctr"))
}

#' @export
#' @rdname new_geo_segment
is_geo_segment <- function(x) {
  inherits(x, "geo_segment")
}

#' @export
#' @rdname new_geo_segment
validate_geo_segment <- function(x) {
  abort("not implemented")
}

#' @export
vec_ptype_abbr.geo_segment <- function(x, ...) {
  "segment"
}

#' @export
format.geo_segment <- function(x, ...) {
  sprintf(
    "(%s %s)\U2192(%s %s)",
    format(field(field(x, "start"), "x"), trim = TRUE, ...),
    format(field(field(x, "start"), "y"), trim = TRUE, ...),
    format(field(field(x, "end"), "x"), trim = TRUE, ...),
    format(field(field(x, "end"), "y"), trim = TRUE, ...)
  )
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.geo_segment <- function(x, ...) {
  as_tibble(vec_data(x), ...)
}

#' @export
#' @importFrom tibble as_tibble
as.data.frame.geo_segment <- function(x, ...) {
  as.data.frame(as_tibble.geo_segment(x, ...))
}

# -------- casting ----------

#' @export
#' @rdname new_geo_segment
as_geo_segment <- function(x, ...) {
  UseMethod("as_geo_segment")
}

#' @export
#' @rdname new_geo_segment
as_geo_segment.default <- function(x, ...) {
  vec_cast(x, geo_segment())
}

#' @method vec_cast geo_segment
#' @export
#' @export vec_cast.geo_segment
#' @rdname new_geo_segment
vec_cast.geo_segment <- function(x, to, ...) {
  UseMethod("vec_cast.geo_segment")
}

#' @method vec_cast.geo_segment default
#' @export
vec_cast.geo_segment.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_segment geo_segment
#' @export
vec_cast.geo_segment.geo_segment <- function(x, to, ...) {
  x
}

#' @method vec_cast.geo_segment geovctrs_wkt
#' @export
vec_cast.geo_segment.geovctrs_wkt <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_segment geovctrs_wkb
#' @export
vec_cast.geo_segment.geovctrs_wkb <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_segment geovctrs_collection
#' @export
vec_cast.geo_segment.geovctrs_collection <- function(x, to, ...) {
  cpp_convert(x, to)
}

# ------------- prototypes ------------

#' @method vec_ptype2 geo_segment
#' @export
#' @export vec_ptype2.geo_segment
#' @rdname new_geo_segment
vec_ptype2.geo_segment <- function(x, y, ...) {
  UseMethod("vec_ptype2.geo_segment", y)
}

#' @method vec_ptype2.geo_segment default
#' @export
vec_ptype2.geo_segment.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geo_segment geo_segment
#' @export
vec_ptype2.geo_segment.geo_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_segment()
}

#' @method vec_ptype2.geo_segment geovctrs_wkt
#' @export
vec_ptype2.geo_segment.geovctrs_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geo_segment geovctrs_wkb
#' @export
vec_ptype2.geo_segment.geovctrs_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geo_segment geovctrs_collection
#' @export
vec_ptype2.geo_segment.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}
