
#' Extract a spatial reference identifier
#'
#' The geovctrs package works with spatial reference identifiers
#' instead of actual CRS objects so that the heavy lifting can
#' be implemented in other packages. However, most geometry
#' formats store an integer spatial reference identifier (SRID)
#' with each feature to propogate this information through
#' calculations. The difference between 0 and `NA` is
#' murky at the moment, but it's likely that 0 will mean
#' "not specified" and `NA` will mean "inherit the other
#' guy's SRID!.
#'
#' @inheritParams geo_bbox
#' @param srid A spatial reference identifier, coerced to
#'   an integer by [as_geo_srid()]. These identifiers can
#'   and should be managed outside of geovctrs except for
#'   0, which is interpreted as "not set".
#'
#' @return An integer vector (one SRID per feature).
#' @export
#'
#' @examples
#' # two points with an SRID
#' geometries <- c(
#'   geo_point(geo_xy(259473, 4876249), srid = 26920),
#'   geo_point(geo_xy(-66, 44), srid = 4326)
#' )
#'
#' geo_srid(geometries)
#' geo_srid(set_geo_srid(geometries, NA))
#'
#' # SRIDs are propogated through conversions,
#' # or discarded with a warning
#' geo_srid(as_geo_wkb(geometries))
#' geo_srid(as_geo_xy(geometries))
#'
geo_srid <- function(x) {
  UseMethod("geo_srid")
}

#' @export
#' @rdname geo_srid
geo_srid.default <- function(x) {
  geo_srid(as_geovctr(x))
}

#' @export
#' @rdname geo_srid
set_geo_srid <- function(x, srid) {
  UseMethod("set_geo_srid")
}

#' @export
geo_srid.vctrs_rcrd <- function(x) {
  field(x, "srid")
}

#' @export
set_geo_srid.vctrs_rcrd <- function(x, srid) {
  srid <- vec_recycle(srid, vec_size(x))
  field(x, "srid") <- as_geo_srid(srid)
  x
}

#' @export
geo_srid.geo_wkt <- function(x) {
  srid <- rep_len(0L, vec_size(x))
  srid[is.na(x)] <- NA_integer_
  srid
}

#' @export
set_geo_srid.geo_wkt <- function(x, srid) {
  if (any(srid != 0)) {
    abort("Can't store SRID with a geo_wkt()")
  }
  x
}

#' @export
geo_srid.geo_xy <- function(x) {
  srid <- rep_len(0L, vec_size(x))
  srid[is.na(x)] <- NA_integer_
  srid
}

#' @export
set_geo_srid.geo_xy <- function(x, srid) {
  if (any(srid != 0, na.rm = TRUE)) {
    abort("Can't store SRID with a geo_xy()")
  }
  x
}

#' @export
geo_srid.geo_wkb <- function(x) {
  cpp_get_srid(x)
}

#' @export
set_geo_srid.geo_wkb <- function(x, srid) {
  srid <- vec_recycle(srid, vec_size(x))
  cpp_set_srid(x, srid)
}

#' @export
#' @rdname geo_srid
as_geo_srid <- function(x) {
  UseMethod("as_geo_srid")
}

#' @export
as_geo_srid.default <- function(x) {
  vec_cast(x, integer())
}
