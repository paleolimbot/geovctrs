
#' Bounding boxes and envelopes
#'
#' Whereas a [geo_bbox()] of a [geovctr][is_geovctr] is always
#' of length 1, a [geo_envelope()] shares the same length as
#' the vector. Both return a [geo_rect()]. Empty vectors
#' (and empty geometries) return `geo_rect(Inf, Inf, -Inf, -Inf)`,
#' and `NA/NaN` values are removed if `na.rm = TRUE` (which might
#' mean more `Inf` values than you expected).
#'
#' @param x A geometry-like object, or one that can be
#'   coerced to a geometry-like object using [as_geovctr()].
#' @param ... Unused
#' @param na.rm Should NAs be removed?
#' @param finite Should only finite values be considered? `TRUE`
#'   implies `na.rm = TRUE`.
#'
#' @return [geo_bbox()] returns a [geo_rect()] of length 1,
#'   [geo_envelope()] returns a [geo_rect()] with the same length as `x`,
#'   `geo_(x|y)_range()` returns a [geo_lim()] of length 1, and
#'   `geo_(x|y)_envelope()` returns a [geo_lim()] with the same length as `x`.
#' @export
#'
#' @examples
#' geo_bbox(wkt(c("POINT (30 10)", "POINT EMPTY")))
#' geo_envelope(wkt(c("POINT (30 10)", "POINT EMPTY")))
#'
geo_bbox <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_bbox")
}

#' @export
geo_bbox.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_bbox(as_geovctr(x), ..., na.rm = na.rm, finite = finite)
}

#' @export
geo_bbox.wk_wkt <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- unclass(wk::wkt_ranges(x, na.rm, finite))[c("xmin", "ymin", "xmax", "ymax")]
  ranges$srid <- summarise_srids(geo_srid(x))
  new_geovctrs_rect(ranges)
}

#' @export
geo_bbox.wk_wkb <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- unclass(wk::wkb_ranges(x, na.rm, finite))[c("xmin", "ymin", "xmax", "ymax")]
  ranges$srid <- summarise_srids(geo_srid(x))
  new_geovctrs_rect(ranges)
}

#' @export
geo_bbox.wk_wksxp <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- unclass(wk::wksxp_ranges(x, na.rm, finite))[c("xmin", "ymin", "xmax", "ymax")]
  ranges$srid <- summarise_srids(geo_srid(x))
  new_geovctrs_rect(ranges)
}

#' @export
geo_bbox.geovctrs_xy <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_bbox(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @export
geo_bbox.geovctrs_xyz <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_bbox(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @export
geo_bbox.geovctrs_segment <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_bbox(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @export
geo_bbox.geovctrs_rect <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_bbox(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @rdname geo_bbox
#' @export
geo_x_range <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_x_range")
}

#' @export
geo_x_range.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  bbox <- vec_data(geo_bbox(x, ..., na.rm  = na.rm, finite = finite))
  new_geovctrs_lim(list(lower = bbox$xmin, upper = bbox$xmax))
}

#' @rdname geo_bbox
#' @export
geo_y_range <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_y_range")
}

#' @export
geo_y_range.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  bbox <- vec_data(geo_bbox(x, ..., na.rm  = na.rm, finite = finite))
  new_geovctrs_lim(list(lower = bbox$ymin, upper = bbox$ymax))
}

#' @rdname geo_bbox
#' @export
geo_envelope <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_envelope")
}

#' @export
geo_envelope.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  restore_geovctr(x, geo_envelope(as_geovctr(x), na.rm = na.rm, finite = finite))
}

#' @export
geo_envelope.wk_wkt <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- unclass(wk::wkt_feature_ranges(x, na.rm, finite))[c("xmin", "ymin", "xmax", "ymax")]
  ranges$srid <- geo_srid(x)
  new_geovctrs_rect(ranges)
}

#' @export
geo_envelope.wk_wkb <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- unclass(wk::wkb_feature_ranges(x, na.rm, finite))[c("xmin", "ymin", "xmax", "ymax")]
  ranges$srid <- geo_srid(x)
  new_geovctrs_rect(ranges)
}

#' @export
geo_envelope.wk_wksxp <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- unclass(wk::wksxp_feature_ranges(x, na.rm, finite))[c("xmin", "ymin", "xmax", "ymax")]
  ranges$srid <- geo_srid(x)
  new_geovctrs_rect(ranges)
}

#' @export
geo_envelope.geovctrs_xy <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  xs <- field(x, "x")
  ys <- field(x, "y")

  if (finite) {
    xs[!is.finite(xs)] <- NA_real_
    ys[!is.finite(ys)] <- NA_real_
    na.rm <- TRUE
  }

  xmin <- xs
  ymin <- ys
  xmax <- xs
  ymax <- ys

  # na.rm is not meaningful here (always TRUE)
  # because geo_xy(NA, NA) is POINT EMPTY
  xmin[is.na(xs)] <- Inf
  ymin[is.na(ys)] <- Inf
  xmax[is.na(xs)] <- -Inf
  ymax[is.na(ys)] <- -Inf

  geo_rect(xmin, ymin, xmax, ymax)
}

#' @export
geo_envelope.geovctrs_segment <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_envelope(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @export
geo_envelope.geovctrs_rect <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_envelope(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @rdname geo_bbox
#' @export
geo_x_envelope <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_x_envelope")
}

#' @export
geo_x_envelope.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  bbox <- vec_data(geo_envelope(x, ..., na.rm  = na.rm, finite = finite))
  new_geovctrs_lim(list(lower = bbox$xmin, upper = bbox$xmax))
}

#' @rdname geo_bbox
#' @export
geo_y_envelope <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_y_envelope")
}

#' @export
geo_y_envelope.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  bbox <- vec_data(geo_envelope(x, ..., na.rm  = na.rm, finite = finite))
  new_geovctrs_lim(list(lower = bbox$ymin, upper = bbox$ymax))
}
