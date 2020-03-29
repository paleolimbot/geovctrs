
#' Collections of coordinate vectors
#'
#' @param ... One or more features. Splicing is supported.
#' @param srid A spatial reference identifier
#'
#' @return A [new_geo_coord_collection()]
#' @export
#'
geo_coord_collection <- function(..., srid = NA) {
  new_geo_coord_collection(rlang::list2(...), srid = vec_cast(srid, integer()))
}

#' S3 Details for coordinate vector collections
#'
#' @inheritParams geo_coord_collection
#' @param x A (possibly) [list()] of geo coordinate vectors
#' @param ... Unused
#'
#' @export
#'
new_geo_coord_collection <- function(x, srid = NA_integer_) {
  vec_assert(x, list())
  new_vctr(x, srid = srid, class = "geo_coord_collection")
}

#' @rdname new_geo_coord_collection
#' @export
validate_geo_coord_collection <- function(x) {
  vec_assert(attr(x, "srid"), integer(), size = 1L)
  lapply(x, function(item) {
    inherits(x, "geo_coord_point") ||
      inherits(x, "geo_coord_multipoint") ||
      inherits(x, "geo_coord_linestring") ||
      inherits(x, "geo_coord_multilinestring") ||
      inherits(x, "geo_coord_polygon") ||
      inherits(x, "geo_coord_multipolygon") ||
      inherits(x, "geo_coord_collection")
  })

  invisible(x)
}

#' @rdname new_geo_coord_collection
#' @export
vec_ptype_abbr.geo_coord_collection <- function(x, ...) {
  "geo_clctn"
}

#' @rdname new_geo_coord_collection
#' @export
format.geo_coord_collection <- function(x, ...) {
  sprintf("<geo_coord_collection[%s]>", length(x))
}

#' @rdname new_geo_coord_collection
#' @export
print.geo_coord_collection <- function(x, ...) {
  cat(sprintf("<geo_coord_collection[%s]>\n", length(x)))
  print(vec_data(x), ...)
}

#' @rdname new_geo_coord_collection
#' @export
is_geo_coord_collection <- function(x) {
  inherits(x, "geo_coord_collection")
}
