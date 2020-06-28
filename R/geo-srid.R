
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
#' geometries <- wkt(
#'   c(
#'     "SRID=26920;POINT (259473 4876249)",
#'     "SRID=4326;POINT (-66 44)"
#'   )
#' )
#'
#' geo_srid(geometries)
#' geo_srid(geo_set_srid(geometries, NA))
#'
#' # SRIDs are propogated through conversions,
#' # or discarded with a warning
#' geo_srid(as_wkb(geometries))
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
geo_set_srid <- function(x, srid) {
  UseMethod("geo_set_srid")
}

#' @export
#' @rdname geo_srid
geo_set_srid.default <- function(x, srid) {
  restore_geovctr(x, geo_set_srid(as_geovctr(x), srid))
}

#' @export
geo_srid.vctrs_rcrd <- function(x) {
  field(x, "srid")
}

#' @export
geo_set_srid.vctrs_rcrd <- function(x, srid) {
  srid <- vec_recycle(srid, vec_size(x))
  field(x, "srid") <- as_geo_srid(srid)
  x
}

#' @export
geo_srid.wk_wkt <- function(x) {
  wk::wkt_meta(x, recursive = FALSE)$srid
}

#' @export
geo_set_srid.wk_wkt <- function(x, srid) {
  recycled <- vec_recycle_common(x, srid)
  new_wk_wkt(cpp_wkt_set_srid(recycled[[1]], recycled[[2]]))
}

#' @export
geo_srid.geovctrs_xy <- function(x) {
  srid <- rep_len(0L, vec_size(x))
  srid[is.na(x)] <- NA_integer_
  srid
}

#' @export
geo_set_srid.geovctrs_xy <- function(x, srid) {
  if (any(srid != 0, na.rm = TRUE)) {
    abort("Can't store SRID with a geo_xy()")
  }
  x
}

#' @export
geo_srid.wk_wkb <- function(x) {
  wk::wkb_meta(x, recursive = FALSE)$srid
}

#' @export
geo_set_srid.wk_wkb <- function(x, srid) {
  recycled <- vec_recycle_common(x, srid)
  new_wk_wkb(cpp_wkb_set_srid(recycled[[1]], recycled[[2]], endian = wk::wk_platform_endian()))
}

#' @export
geo_srid.wk_wksxp <- function(x) {
  wk::wksxp_meta(x, recursive = FALSE)$srid
}

#' @export
geo_set_srid.wk_wksxp <- function(x, srid) {
  recycled <- vec_recycle_common(x, srid)
  new_wk_wksxp(cpp_wksxp_set_srid(recycled[[1]], recycled[[2]]))
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
