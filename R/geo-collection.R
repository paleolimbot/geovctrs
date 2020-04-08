
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
  # make it possible to create a nested geovctrs_collection
  if (is_geo_collection(feature)) {
    feature = list(feature)
  }

  collection <- new_geovctrs_collection(
    vec_recycle_common(
      feature = feature,
      srid = as_geo_srid(srid)
    )
  )

  validate_geo_collection(collection)
  collection
}

#' @rdname geo_collection
#' @export
geo_point <- function(xy, srid = 0)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  point <- new_geovctrs_point(list(xy = xy))
  validate_geo_point(point)
  new_geovctrs_collection(list(feature = list(point), srid = as_geo_srid(srid)))
}

new_geovctrs_point <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_point")
}

validate_geo_point <-function(x) {
  stopifnot(vec_size(x$xy) %in% c(0, 1))
  invisible(x)
}

#' @rdname geo_collection
#' @export
geo_linestring <- function(xy, srid = 0)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  feat <- new_geovctrs_linestring(list(xy = xy))
  validate_geo_linestring(feat)
  new_geovctrs_collection(list(feature = list(feat), srid = as_geo_srid(srid)))
}

new_geovctrs_linestring <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_linestring")
}

validate_geo_linestring <-function(x) {
  stopifnot(vec_size(x$xy) != 1)
  invisible(x)
}

#' @rdname geo_collection
#' @export
geo_polygon <- function(xy, ring = 1L, srid = 0)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  feat <- new_geovctrs_polygon(as_part_identifier(xy, ring = ring))
  validate_geo_polygon(feat)
  new_geovctrs_collection(list(feature = list(feat), srid = as_geo_srid(srid)))
}

new_geovctrs_polygon <- function(x) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$ring, integer())
  structure(x, class = "geo_polygon")
}

validate_geo_polygon <-function(x) {
  rings <- split(x$xy, x$ring)
  lengths <- vapply(rings, length, integer(1))
  stopifnot(!any(lengths %in% c(1, 2)))

  invisible(x)
}

#' @rdname geo_collection
#' @export
geo_multipoint <- function(feature, srid = 0) {
  feature <- vec_cast(feature, geo_collection())
  values <- field(feature, "feature")
  is_point <- vapply(values, inherits, "geo_point", FUN.VALUE = logical(1))
  if (!all(is_point)) {
    abort("All features must be `geo_point()`s")
  }

  xy <- lapply(values, `[[`, "xy")

  feat <- new_geovctrs_multipoint(
    list(
      xy = vec_c(!!!xy, .ptype = geo_xy())
    )
  )

  new_geovctrs_collection(list(feature = list(feat), srid = as_geo_srid(srid)))
}

new_geovctrs_multipoint <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_multipoint")
}

#' @rdname geo_collection
#' @export
geo_multilinestring <- function(feature, srid = 0) {
  feature <- vec_cast(feature, geo_collection())
  values <- field(feature, "feature")
  is_linestring <- vapply(values, inherits, "geo_linestring", FUN.VALUE = logical(1))
  if (!all(is_linestring)) {
    abort("All features must be `geo_linestring()`s")
  }

  xy <- lapply(values, `[[`, "xy")
  lengths <- vapply(xy, length, integer(1))
  part_rle <- structure(list(lengths = lengths, values = seq_along(lengths)), class = "rle")

  feat <- new_geovctrs_multilinestring(
    list(
      xy = vec_c(!!!xy, .ptype = geo_xy()),
      part = inverse.rle(part_rle)
    )
  )

  new_geovctrs_collection(list(feature = list(feat), srid = as_geo_srid(srid)))
}

new_geovctrs_multilinestring <- function(x) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$part, integer())
  structure(x, class = "geo_multilinestring")
}

#' @rdname geo_collection
#' @export
geo_multipolygon <- function(feature, srid = 0) {
  feature <- vec_cast(feature, geo_collection())
  values <- field(feature, "feature")
  is_polygon <- vapply(values, inherits, "geo_polygon", FUN.VALUE = logical(1))
  if (!all(is_polygon)) {
    abort("All features must be `geo_polygon()`s")
  }

  xy <- lapply(values, `[[`, "xy")
  ring <- lapply(values, `[[`, "ring")
  lengths <- vapply(xy, length, integer(1))
  part_rle <- structure(list(lengths = lengths, values = seq_along(lengths)), class = "rle")

  feat <- new_geovctrs_multipolygon(
    list(
      xy = vec_c(!!!xy, .ptype = geo_xy()),
      part = inverse.rle(part_rle),
      ring = vec_c(!!!ring, .ptype = integer())
    )
  )

  new_geovctrs_collection(list(feature = list(feat), srid = as_geo_srid(srid)))
}

new_geovctrs_multipolygon <- function(x) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$part, integer())
  vec_assert(x$ring, integer())
  structure(x, class = "geo_multipolygon")
}

#' S3 Details for coordinate vector collections
#'
#' @param x A (possibly) [geo_collection()]
#' @inheritParams new_geovctrs_wkt
#'
#' @export
#'
new_geovctrs_collection <- function(x = list(feature = list(), srid = integer())) {
  vec_assert(x$feature, list())
  new_rcrd(x, class = c("geovctrs_collection", "geovctr"))
}

#' @rdname new_geovctrs_collection
#' @export
validate_geo_collection <- function(x) {
  lapply(field(x, "feature"), function(item) {
    is.null(item) ||
      inherits(item, "geo_coord_point") ||
      inherits(item, "geo_coord_multipoint") ||
      inherits(item, "geo_coord_linestring") ||
      inherits(item, "geo_coord_multilinestring") ||
      inherits(item, "geo_coord_polygon") ||
      inherits(item, "geo_coord_multipolygon") ||
      inherits(item, "geovctrs_collection")
  })

  invisible(x)
}

#' @export
vec_ptype_abbr.geovctrs_collection <- function(x, ...) {
  "clctn"
}

#' @export
format.geovctrs_collection <- function(x, ...) {
  geo_format(x, ...)
}

#' @export
print.geovctrs_collection <- function(x, ...) {
  geo_print(x, ...)
}

#' @rdname new_geovctrs_collection
#' @export
is_geo_collection <- function(x) {
  inherits(x, "geovctrs_collection")
}

#' @rdname new_geovctrs_collection
#' @export
as_geo_collection <- function(x, ...) {
  UseMethod("as_geo_collection")
}

#' @rdname new_geovctrs_collection
#' @export
as_geo_collection.default <- function(x, ...) {
  vec_cast(x, geo_collection())
}

#' @method vec_cast geovctrs_collection
#' @export
#' @export vec_cast.geovctrs_collection
#' @rdname new_geovctrs_collection
vec_cast.geovctrs_collection <- function(x, to, ...) {
  UseMethod("vec_cast.geovctrs_collection")
}

#' @method vec_cast.geovctrs_collection default
#' @export
vec_cast.geovctrs_collection.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geovctrs_collection geovctrs_collection
#' @export
vec_cast.geovctrs_collection.geovctrs_collection <- function(x, to, ...) {
  x
}

#' @method vec_cast.geovctrs_collection geovctrs_xy
#' @export
vec_cast.geovctrs_collection.geovctrs_xy <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_collection geovctrs_rect
#' @export
vec_cast.geovctrs_collection.geovctrs_rect <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_collection geovctrs_segment
#' @export
vec_cast.geovctrs_collection.geovctrs_segment <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_collection geovctrs_wkt
#' @export
vec_cast.geovctrs_collection.geovctrs_wkt <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_collection geovctrs_wkb
#' @export
vec_cast.geovctrs_collection.geovctrs_wkb <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_ptype2 geovctrs_collection
#' @export
#' @export vec_ptype2.geovctrs_collection
#' @rdname new_geovctrs_collection
vec_ptype2.geovctrs_collection <- function(x, y, ...) {
  UseMethod("vec_ptype2.geovctrs_collection", y)
}

#' @method vec_ptype2.geovctrs_collection default
#' @export
vec_ptype2.geovctrs_collection.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geovctrs_collection geovctrs_wkb
#' @export
vec_ptype2.geovctrs_collection.geovctrs_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geovctrs_collection geovctrs_wkt
#' @export
vec_ptype2.geovctrs_collection.geovctrs_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geovctrs_collection geovctrs_collection
#' @export
vec_ptype2.geovctrs_collection.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geovctrs_collection geovctrs_xy
#' @export
vec_ptype2.geovctrs_collection.geovctrs_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geovctrs_collection geovctrs_segment
#' @export
vec_ptype2.geovctrs_collection.geovctrs_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}

#' @method vec_ptype2.geovctrs_collection geovctrs_rect
#' @export
vec_ptype2.geovctrs_collection.geovctrs_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}
