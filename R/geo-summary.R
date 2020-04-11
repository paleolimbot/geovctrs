
#' Feature-level geometry summaries
#'
#' @inheritParams geo_bbox
#'
#' @return A [tibble::tibble()] with columns `is_empty`, `geometry_type`,
#'   `n_geometries`, `n_coordinates`, `srid`, `coordinate_dimensions`,
#'   `first_coordinate`, `problems`, and `is_missing`.
#' @export
#'
#' @examples
#' geo_summary(geo_nc)
#'
#' geo_summary(
#'   geo_wkt(
#'     c(
#'       "POINT (30 10)",
#'       "LINESTRING (0 0, 10 11)",
#'       "POLYGON EMPTY"
#'     )
#'   )
#' )
#'
geo_summary <- function(x) {
  UseMethod("geo_summary")
}

#' @export
geo_summary.default <- function(x) {
  geo_summary(as_geovctr(x))
}

#' @export
geo_summary.geovctr <- function(x) {
  cpp_summary <- geovctrs_cpp_summary(x)
  cpp_summary$geometry_type <- c(
    "point", "linestring", "linearring", "polygon",
    "multipoint", "multilinestring", "multipolygon",
    "geometrycollection"
  )[cpp_summary$geometry_type + 1]

  # don't error for problems, but be very noisy about them
  if (any(!is.na(cpp_summary$problems))) {
    n_probs <- sum(!is.na(cpp_summary$problems))
    rlang::warn(
      sprintf(
        "%s geometr%s failed to parse. See geo_summary(x)$problems for details.",
        n_probs, if (n_probs > 1) "ies" else "y"
      )
    )
  }

  as_tibble(cpp_summary)
}

#' @rdname geo_summary
#' @export
geo_n_geometries <- function(x) {
  UseMethod("geo_n_geometries")
}

#' @export
geo_n_geometries.default <- function(x) {
  geo_summary(x)$n_geometries
}

#' @rdname geo_summary
#' @export
geo_n_coordinates <- function(x) {
  UseMethod("geo_n_coordinates")
}

#' @export
geo_n_coordinates.default <- function(x) {
  geo_summary(x)$n_coordinates
}

#' @rdname geo_summary
#' @export
geo_geometry_type <- function(x) {
  UseMethod("geo_geometry_type")
}

#' @export
geo_geometry_type.default <- function(x) {
  geo_summary(x)$geometry_type
}

#' @rdname geo_summary
#' @export
geo_coordinate_dimensions <- function(x) {
  UseMethod("geo_coordinate_dimensions")
}

#' @export
geo_coordinate_dimensions.default <- function(x) {
  geo_summary(x)$coordinate_dimensions
}

#' @rdname geo_summary
#' @export
geo_first_coordinate <- function(x) {
  UseMethod("geo_first_coordinate")
}

#' @export
geo_first_coordinate.default <- function(x) {
  geo_summary(x)$first_coordinate
}
