
#' Feature-level geometry summaries
#'
#' @inheritParams geo_bbox
#'
#' @return A [tibble::tibble()] with columns `geometry_type`, `is_empty`,
#'   `n_coordinates`, `srid`, `coordinate_dimensions`, and `first_coordinate`.
#' @export
#'
#' @examples
#' geo_summary(
#'   geo_wkt(
#'     c(
#'     "POINT (30 10)",
#'     "LINESTRING (0 0, 10 11)",
#'     "POLYGON EMPTY"
#'    )
#'  )
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
  tibble::tibble(
    geometry_type = geo_geometry_type(x),
    is_empty = geo_is_empty(x),
    n_coordinates = geo_n_coordinates(x),
    n_geometries =  geo_n_geometries(x),
    srid = geo_srid(x),
    coordinate_dimensions = geo_coordinate_dimensions(x),
    first_coordinate = geo_first_coordinate(x)
  )
}

#' @rdname geo_summary
#' @export
geo_n_geometries <- function(x) {
  UseMethod("geo_n_geometries")
}

#' @export
geo_n_geometries.default <- function(x) {
  geo_n_geometries(as_geovctr(x))
}

#' @export
geo_n_geometries.geovctr <- function(x) {
  cpp_n_geometries(x)
}

#' @rdname geo_summary
#' @export
geo_n_coordinates <- function(x) {
  UseMethod("geo_n_coordinates")
}

#' @export
geo_n_coordinates.default <- function(x) {
  geo_n_coordinates(as_geovctr(x))
}

#' @export
geo_n_coordinates.geovctr <- function(x) {
  cpp_n_coordinates(x)
}

#' @rdname geo_summary
#' @export
geo_geometry_type <- function(x) {
  UseMethod("geo_geometry_type")
}

#' @export
geo_geometry_type.default <- function(x) {
  geo_geometry_type(as_geovctr(x))
}

#' @export
geo_geometry_type.geovctr <- function(x) {
  c(
    "point", "linestring", "linearring", "polygon",
    "multipoint", "multilinestring", "multipolygon",
    "geometrycollection"
  )[cpp_geom_type_id(x) + 1]
}

#' @rdname geo_summary
#' @export
geo_coordinate_dimensions <- function(x) {
  UseMethod("geo_coordinate_dimensions")
}

#' @export
geo_coordinate_dimensions.default <- function(x) {
  geo_coordinate_dimensions(as_geovctr(x))
}

#' @export
geo_coordinate_dimensions.geovctr <- function(x) {
  cpp_coordinate_dimensions(x)
}

#' @rdname geo_summary
#' @export
geo_first_coordinate <- function(x) {
  UseMethod("geo_first_coordinate")
}

#' @export
geo_first_coordinate.default <- function(x) {
  geo_first_coordinate(as_geovctr(x))
}

#' @export
geo_first_coordinate.geovctr <- function(x) {
  cpp_first_coordinate(x)
}
