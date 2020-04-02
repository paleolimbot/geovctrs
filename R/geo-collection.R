
#' Geometry constructors
#'
#' A [geo_collection()] is an in-memory R-native format that can store most
#' geometries. It is used to power [geo_plot()] in addition to providing
#' constructors for geometries from data frames (possibly using
#' [dplyr::group_by()] and [dplyr::summarise()]). Collections contain zero
#' or more objects of type [geo_point()], [geo_linestring()], [geo_polygon()],
#' [geo_multipoint()], [geo_multilinestring()], and/or
#' [geo_multipolygon()].
#'
#' @param feature A list of one or more features.
#' @inheritParams geo_srid
#'
#' @return A [new_geo_collection()]
#' @export
#'
geo_collection <- function(feature = list(), srid = 0) {
  new_geo_collection(
    vec_recycle_common(
      feature = feature,
      srid = as_geo_srid(srid)
    )
  )
}

#' S3 Details for coordinate vector collections
#'
#' @param x A (possibly) [geo_collection()]
#' @inheritParams new_geo_wkt
#'
#' @export
#'
new_geo_collection <- function(x = list(feature = list(), srid = integer())) {
  vec_assert(x$feature, list())
  new_rcrd(x, class = c("geo_collection", "geovctr"))
}

#' @rdname new_geo_collection
#' @export
validate_geo_collection <- function(x) {
  lapply(x, function(item) {
    inherits(x, "geo_coord_point") ||
      inherits(x, "geo_coord_multipoint") ||
      inherits(x, "geo_coord_linestring") ||
      inherits(x, "geo_coord_multilinestring") ||
      inherits(x, "geo_coord_polygon") ||
      inherits(x, "geo_coord_multipolygon") ||
      inherits(x, "geo_collection")
  })

  invisible(x)
}

#' @export
vec_ptype_abbr.geo_collection <- function(x, ...) {
  "geo_clctn"
}

#' @export
format.geo_collection <- function(x, ..., top_level = TRUE) {
  if (top_level) {
    vapply(field(x, "feature"), format, ..., top_level = FALSE, FUN.VALUE = character(1))
  } else {
    sprintf("<geo_collection[%s]>", length(x))
  }
}

#' @rdname new_geo_collection
#' @export
is_geo_collection <- function(x) {
  inherits(x, "geo_collection")
}

#' @rdname new_geo_collection
#' @export
as_geo_collection <- function(x, ...) {
  UseMethod("as_geo_collection")
}

#' @rdname new_geo_collection
#' @export
as_geo_collection.default <- function(x, ...) {
  vec_cast(x, geo_collection())
}

#' @method vec_cast geo_collection
#' @export
#' @export vec_cast.geo_collection
#' @rdname new_geo_collection
vec_cast.geo_collection <- function(x, to, ...) {
  UseMethod("vec_cast.geo_collection")
}

#' @method vec_cast.geo_collection default
#' @export
vec_cast.geo_collection.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_collection geo_collection
#' @export
vec_cast.geo_collection.geo_collection <- function(x, to, ...) {
  x
}

#' @method vec_cast.geo_collection geo_xy
#' @export
vec_cast.geo_collection.geo_xy <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_collection geo_rect
#' @export
vec_cast.geo_collection.geo_rect <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_collection geo_segment
#' @export
vec_cast.geo_collection.geo_segment <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_collection geo_wkt
#' @export
vec_cast.geo_collection.geo_wkt <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_collection geo_wkb
#' @export
vec_cast.geo_collection.geo_wkb <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_ptype2 geo_collection
#' @export
#' @export vec_ptype2.geo_collection
#' @rdname new_geo_collection
vec_ptype2.geo_collection <- function(x, y, ...) {
  UseMethod("vec_ptype2.geo_collection", y)
}

#' @method vec_ptype2.geo_collection default
#' @export
vec_ptype2.geo_collection.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.geo_collection geo_wkb
#' @export
vec_ptype2.geo_collection.geo_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geo_collection geo_wkt
#' @export
vec_ptype2.geo_collection.geo_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geo_collection geo_collection
#' @export
vec_ptype2.geo_collection.geo_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geo_collection geo_xy
#' @export
vec_ptype2.geo_collection.geo_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geo_collection geo_segment
#' @export
vec_ptype2.geo_collection.geo_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geo_collection geo_rect
#' @export
vec_ptype2.geo_collection.geo_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}
