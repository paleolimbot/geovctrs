
#' Feature-level geometry summaries
#'
#' @inheritParams geo_bbox
#'
#' @return A [tibble::tibble()] with columns `is_empty`, `geometry_type`,
#'   `n_geometries`, `n_coordinates`, `srid`, `coordinate_dimensions`,
#'   `has_z`, `first_coordinate`, `problems`, and `is_missing`.
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
  # for now...
  geo_summary(as_geo_wkt(x))
}

#' @export
geo_summary.geovctrs_wkt <- function(x) {
  wk_summary <- wk::wkt_meta(x)

  tibble(
    is_empty = wk_summary$size > 0,
    geometry_type = wk::wk_geometry_type(wk_summary$type_id),
    size = wk_summary$size,
    srid = wk_summary$srid,
    has_z = wk_summary$has_z,
    has_m = wk_summary$has_m
  )
}

#' @export
geo_summary.geovctrs_wkb <- function(x) {
  wk_summary <- wk::wkb_meta(x)

  tibble(
    is_empty = wk_summary$size > 0,
    geometry_type = wk::wk_geometry_type(wk_summary$type_id),
    size = wk_summary$size,
    srid = wk_summary$srid,
    has_z = wk_summary$has_z,
    has_m = wk_summary$has_m
  )
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
