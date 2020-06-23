
#' Geometry constructors
#'
#' A [geo_collection()] is an in-memory R-native format that can store most
#' geometries. It is used to power [geo_plot()] in addition to providing
#' constructors for geometries from data frames (possibly using
#' [dplyr::group_by()] and [dplyr::summarise()]). Collections contain zero
#' or more objects of type [geo_point()], [geo_linestring()], [geo_polygon()],
#' [geo_multipoint()], [geo_multilinestring()], and/or
#' [geo_multipolygon()].
#'
#' @param feature A [geo_collection()] of one or more features.
#'   for multi geometries, this must be a collection that only contains
#'   that type (e.g., multipolygons can only be composed of polygons).
#' @param xy A [geo_xy()] of coordinates
#' @param ring A vector whose unique values separate rings. Row order
#'   matters: the first value encountered will identify the outer ring.
#' @inheritParams geo_srid
#'
#' @return A [new_geovctrs_collection()] of length 1.
#' @export
#'
#' @examples
#' # geo_point() and family all return a geo_collection() of length 1
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
geo_collection <- function(feature = list(), srid = 0) {
  abort("Not implemented")
}

#' @rdname geo_collection
#' @export
geo_point <- function(xy, srid = 0)  {
  xy <- cast_xy_or_xyz(xy)
  stopifnot(vec_size(srid) == 1)


}

#' @rdname geo_collection
#' @export
geo_linestring <- function(xy, srid = 0)  {
  xy <- cast_xy_or_xyz(xy)
  stopifnot(vec_size(srid) == 1)


}

#' @rdname geo_collection
#' @export
geo_polygon <- function(xy, ring = 1L, srid = 0)  {
  xy <- cast_xy_or_xyz(xy)
  stopifnot(vec_size(srid) == 1)


}

#' @rdname geo_collection
#' @export
geo_multipoint <- function(feature, srid = 0) {
  abort("Not implemented")
}

#' @rdname geo_collection
#' @export
geo_multilinestring <- function(feature, srid = 0) {
  abort("Not implemented")
}

#' @rdname geo_collection
#' @export
geo_multipolygon <- function(feature, srid = 0) {
  abort("Not implemented")
}


# utils just for collections
cast_xy_or_xyz <- function(x) {
  if (is_geovctrs_xy(x)) {
    x
  } else if (is_geovctr(x)) {
    # this particular conversion should give an XYZ to avoid data loss
    geovctrs_cpp_convert(x, geo_xy())
  } else {
    vec_cast(x, geo_xy())
  }
}

assert_has_xy_or_xyz <- function(x) {
  if (!inherits(x$xy, "geovctrs_xy")) {
    abort("`xy` must be a `geo_xy()` or a `geo_xyz()`")
  }
  invisible(x)
}
