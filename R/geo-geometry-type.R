
#' Extract feature geometry type
#'
#' @inheritParams geo_bbox
#'
#' @return A character vector with lowercase geometry types
#' @export
#'
#' @examples
#' geo_geometry_type("POINT EMPTY")
#'
geo_geometry_type <- function(x) {
  UseMethod("geo_geometry_type")
}

#' @rdname geo_geometry_type
#' @export
geo_geometry_type.default <- function(x) {
  geo_geometry_type(as_geovctr(x))
}

#' @rdname geo_geometry_type
#' @export
geo_geometry_type.geovctr <- function(x) {
  geo_geometry_type(as_geo_wkb(x))
}

#' @rdname geo_geometry_type
#' @export
geo_geometry_type.geovctrs_wkt <- function(x) {
  wk::wk_geometry_type(wk::wkt_streamer_meta(x)$type_id)
}

#' @rdname geo_geometry_type
#' @export
geo_geometry_type.geovctrs_wkb <- function(x) {
  wk::wk_geometry_type(wk::wkb_meta(x)$type_id)
}
