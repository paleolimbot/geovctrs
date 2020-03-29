
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
#' @param ... Unused
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
format.geo_collection <- function(x, ...) {
  sprintf("<geo_collection[%s]>", length(x))
}

#' @export
print.geo_collection <- function(x, ...) {
  cat(sprintf("<geo_collection[%s]>\n", length(x)))
  print(vec_data(x), ...)
}

#' @rdname new_geo_collection
#' @export
is_geo_collection <- function(x) {
  inherits(x, "geo_collection")
}
