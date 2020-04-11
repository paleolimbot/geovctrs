
#' Extract coordinates as a tibble
#'
#' @inheritParams geo_bbox
#'
#' @return A tibble with one row per coordinate and columns
#'   `feature` and `xy`. Vectors that include mutli geometries
#'   will have a `part` column, and vectors that include polygons
#'   will have a `ring` column.
#' @export
#'
#' @examples
#' geo_coordinates("POINT (20 17)")
#' geo_coordinates("POINT EMPTY")
#'
#' geo_coordinates("LINESTRING EMPTY")
#' geo_coordinates("LINESTRING (30 10, 0 0)")
#'
#' geo_coordinates("POLYGON EMPTY")
#' geo_coordinates("POLYGON ((30 10, 0 0, -30 10, 30 10))")
#'
#' geo_coordinates(
#'   "
#'   MULTIPOLYGON (
#'     ((40 40, 20 45, 45 30, 40 40)),
#'     ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),
#'       (30 20, 20 15, 20 25, 30 20))
#'   )"
#' )
#'
geo_coordinates <- function(x, ...) {
  UseMethod("geo_coordinates")
}

#' @export
geo_coordinates.default <- function(x, ...) {
  geo_coordinates(as_geovctr(x), ...)
}

#' @export
geo_coordinates.geovctr <- function(x, ...) {
  geo_coordinates(as_geo_collection(x), ...)
}

#' @export
geo_coordinates.geovctrs_collection <- function(x, ...) {
  features <- field(x, "feature")
  is_nested_collection <- vapply(features, inherits, "geovctrs_collection", FUN.VALUE = logical(1))
  is_empty <- geo_is_empty(x)
  is_null <- vapply(features, is.null, logical(1))
  is_empty_collection <- is_nested_collection & is_empty

  if (any(!is_empty & is_nested_collection)) {
    abort("Can't return coordinates of a nested geometry collection")
  }

  features <- lapply(seq_along(features), function(i) {
    x <- unclass(features[[i]])
    if (!is.null(x)) {
      x$feature <- i
    }

    x
  })

  features <- lapply(features[!is_null & !is_empty_collection], as_tibble)
  if (length(features) == 0) {
    tibble(feature = integer(0), xy = geo_xy())
  } else {
    out <- vec_rbind(!!!features)
    out[intersect(c("feature", "xy", "part", "ring"), names(out))]
  }
}
