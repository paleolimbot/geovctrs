
#' Geometry constructors
#'
#' These functions provide a means to construct geometries from data
#' frames, possibly using
#' [dplyr::group_by()] and [dplyr::summarise()]. Collections contain zero
#' or more objects of type [geo_point()], [geo_linestring()], [geo_polygon()],
#' [geo_multipoint()], [geo_multilinestring()], and/or
#' [geo_multipolygon()]. See [wkutils::coords_point_translate_wkb()] and related
#' functions for high-performance methods to create these vectors.
#'
#' @param feature A vector of one or more features.
#'   For multi geometries, this must be a collection that only contains
#'   that type (e.g., multipolygons can only be composed of polygons).
#' @param xy A [geo_xy()] of coordinates
#' @param ring A vector whose unique values separate rings. Row order
#'   matters: the first value encountered will identify the outer ring.
#' @inheritParams geo_srid
#'
#' @return A [wk::wksxp()] of length 1.
#' @export
#'
#' @examples
#' # geo_point() and family all return a wk::wksxp() of length 1
#' c(geo_point(geo_xy(0, 1)), geo_point(geo_xy(1, 2)))
#'
#' # linestring
#' geo_linestring(geo_xy(1:5, 2:6))
#'
#' # a polygon
#' geo_polygon(geo_xy(c(0, 10, 0, 0), c(0, 0, 10, 0)))
#'
#' # polygon with a hole
#' poly_hole <- geo_polygon(
#'   geo_xy(
#'     c(35, 45, 15, 10, 35, 20, 35, 30, 20),
#'     c(10, 45, 40, 20, 10, 30, 35, 20, 30)
#'   ),
#'   ring = c(1, 1, 1, 1, 1, 2, 2, 2, 2)
#' )
#'
#' # multipoint
#' geo_multipoint(
#'   c(geo_point(geo_xy(10, 30)), geo_point(geo_xy(12, 11)))
#' )
#'
#' # multilinestring
#' geo_multilinestring(
#'   c(
#'     geo_linestring(geo_xy(0:1, 0:1)),
#'     geo_linestring(geo_xy(c(12, 30), c(11, 10)))
#'   )
#' )
#'
#' # multipolygon
#' geo_multipolygon(
#'   geo_polygon(geo_xy(c(0, 10, 0, 0), c(0, 0, 10, 0)))
#' )
#'
#' # nested geo_collection()
#' c(geo_point(geo_xy(0, 1)), geo_collection(geo_point(geo_xy(1, 2))))
#'
geo_collection <- function(feature = wksxp(), srid = NA) {
  feature <- structure(as_wksxp(feature), class = "wk_geometrycollection")
  construct_wksxp(feature, srid)
}

#' @rdname geo_collection
#' @export
geo_point <- function(xy = geo_xy(), srid = NA)  {
  if (!inherits(xy, "geovctrs_xy")) {
    xy <- as_geo_xy(xy)
  }

  feature <- structure(as.matrix(xy), class = "wk_point")
  attr(feature, "has_z") <- inherits(xy, "geovctrs_xyz")
  construct_wksxp(feature, srid)
}

#' @rdname geo_collection
#' @export
geo_linestring <- function(xy = geo_xy(), srid = NA)  {
  if (!inherits(xy, "geovctrs_xy")) {
    xy <- as_geo_xy(xy)
  }

  feature <- structure(as.matrix(xy), class = "wk_linestring")
  attr(feature, "has_z") <- inherits(xy, "geovctrs_xyz")
  construct_wksxp(feature, srid)
}

#' @rdname geo_collection
#' @export
geo_polygon <- function(xy = geo_xy(), ring = 1L, srid = NA)  {
  if (vec_size(xy) == 0) {
    return(new_wk_wksxp(list(structure(list(), class = "wk_polygon"))))
  }
  stopifnot(vec_size(srid) == 1)

  if (!inherits(xy, "geovctrs_xy")) {
    xy <- as_geo_xy(xy)
  }

  data <- vec_data(xy)
  raw_wksxp <- wkutils::coords_polygon_translate_wksxp(
    data$x,
    data$y,
    data$z %||% NA_real_,
    ring_id = ring
  )

  if (!is.na(srid)) {
    attr(raw_wksxp[[1]], "srid") <- as_geo_srid(srid)
  }

  new_wk_wksxp(raw_wksxp)
}

#' @rdname geo_collection
#' @export
geo_multipoint <- function(feature = wksxp(), srid = NA) {
  construct_multi_type(feature, type_id = 1, srid = srid)
}

#' @rdname geo_collection
#' @export
geo_multilinestring <- function(feature = wksxp(), srid = NA) {
  construct_multi_type(feature, type_id = 2, srid = srid)
}

#' @rdname geo_collection
#' @export
geo_multipolygon <- function(feature = wksxp(), srid = NA) {
  construct_multi_type(feature, type_id = 3, srid = srid)
}

construct_multi_type <- function(feature, type_id, srid) {
  type <- wkutils::wk_geometry_type(type_id)
  feature <- as_wksxp(feature)
  feature_meta <- wkutils::wksxp_meta(feature)
  if (!all(feature_meta$type_id == type_id)) {
    abort(sprintf("All elements of `feature` must be of type '%s'", type))
  }

  construct_wksxp(structure(feature, class = paste0("wk_multi", type)), srid)
}

construct_wksxp <- function(feature, srid) {
  stopifnot(vec_size(srid) == 1)
  if (!is.na(srid)) {
    attr(feature, "srid") <- as_geo_srid(srid)
  }

  wksxp(list(feature))
}
