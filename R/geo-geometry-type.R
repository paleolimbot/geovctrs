
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
  geo_geometry_type(as_wksxp(as_geovctr(x)))
}

#' @rdname geo_geometry_type
#' @export
geo_geometry_type.wk_wkt <- function(x) {
  wkutils::wk_geometry_type(wkutils::wkt_streamer_meta(x)$type_id)
}

#' @rdname geo_geometry_type
#' @export
geo_geometry_type.wk_wkb <- function(x) {
  wkutils::wk_geometry_type(wkutils::wkb_meta(x)$type_id)
}

#' @rdname geo_geometry_type
#' @export
geo_geometry_type.wk_wksxp <- function(x) {
  wkutils::wk_geometry_type(wkutils::wksxp_meta(x)$type_id)
}
