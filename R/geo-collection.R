
#' Collections of coordinate vectors
#'
#' @param feature A list of one or more features.
#' @param srid A spatial reference identifier
#'
#' @return A [new_geo_collection()]
#' @export
#'
geo_collection <- function(feature = list(), srid = NA) {
  new_geo_collection(
    vec_recycle_common(
      feature = feature,
      srid = vec_cast(srid, integer())
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
  new_rcrd(x, class = "geo_collection")
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
#' @rdname new_geo_collection
vec_cast.geo_collection.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_collection geo_collection
#' @export
#' @rdname new_geo_collection
vec_cast.geo_collection.geo_collection <- function(x, to, ...) {
  x
}

#' @method vec_cast.geo_collection geo_xy
#' @export
#' @rdname new_geo_collection
vec_cast.geo_collection.geo_xy <- function(x, to, ...) {
  geo_convert(x, geo_collection())
}
