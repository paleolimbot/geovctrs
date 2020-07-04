
#' Work with Z values
#'
#' Currently, geovctrs supports X, Y, and Z coordinates. These functions extract,
#' set, and drop Z values.
#'
#' @inheritParams geo_bbox
#' @param z A vector of z coordinate values.
#'
#' @return A modified version of `x`.
#' @export
#'
#' @examples
#' geo_set_z(c("POINT (2 3)", "POINT Z (2 3 4)"), 10)
#' geo_drop_z(c("POINT (2 3)", "POINT Z (2 3 4)"))
#' geo_has_z(c("POINT (2 3)", "POINT Z (2 3 4)"))
#' geo_z_range(c("POINT (2 3)", "POINT Z (2 3 4)"), na.rm = TRUE)
#' geo_z_envelope(c("POINT (2 3)", "POINT Z (2 3 4)"), na.rm = TRUE)
#'
geo_set_z <- function(x, z) {
  UseMethod("geo_set_z")
}

#' @export
geo_set_z.default <- function(x, z) {
  restore_geovctr(x, geo_set_z(as_geovctr(x), z))
}

#' @export
geo_set_z.wk_wksxp <- function(x, z) {
  recycled <- vec_recycle_common(x, z)
  new_wk_wksxp(wkutils::wksxp_set_z(recycled[[1]], recycled[[2]]))
}

#' @export
geo_set_z.wk_wkb <- function(x, z) {
  recycled <- vec_recycle_common(x, z)
  new_wk_wkb(wkutils::wkb_set_z(recycled[[1]], recycled[[2]]))
}

#' @export
geo_set_z.wk_wkt <- function(x, z) {
  recycled <- vec_recycle_common(x, z)
  new_wk_wkt(wkutils::wkt_set_z(recycled[[1]], recycled[[2]]))
}

#' @export
geo_set_z.geovctrs_xy <- function(x, z) {
  if (all(is.na(z))) {
    x
  } else {
    geo_xyz(field(x, "x"), field(x, "y"), z)
  }
}

#' @export
geo_set_z.geovctrs_xyz <- function(x, z) {
  if (all(is.na(z))) {
    geo_drop_z(x)
  } else {
    geo_xyz(field(x, "x"), field(x, "y"), z)
  }
}

#' @rdname geo_set_z
#' @export
geo_drop_z <- function(x) {
  UseMethod("geo_drop_z")
}

#' @export
geo_drop_z.default <- function(x) {
  geo_set_z(x, NA_real_)
}

#' @export
geo_drop_z.geovctrs_xyz <- function(x) {
  x <- vec_data(x)
  x$z <- NULL
  new_geovctrs_xy(x)
}

#' @export
geo_drop_z.geovctrs_xy <- function(x) {
  x
}

#' @rdname geo_set_z
#' @export
geo_has_z <- function(x) {
  UseMethod("geo_has_z")
}

#' @export
geo_has_z.default <- function(x) {
  geo_has_z(as_geovctr(x))
}

#' @export
geo_has_z.wk_wkb <- function(x) {
  wkutils::wkb_meta(x)$has_z
}

#' @export
geo_has_z.wk_wkt <- function(x) {
  wkutils::wkt_meta(x)$has_z
}

#' @export
geo_has_z.wk_wksxp <- function(x) {
  wkutils::wksxp_meta(x)$has_z
}

#' @export
geo_has_z.geovctrs_xy <- function(x) {
  rep_len(FALSE, length(x))
}

#' @export
geo_has_z.geovctrs_xyz <- function(x) {
  !is.na(field(x, "z"))
}

#' @export
geo_has_z.geovctrs_segment <- function(x) {
  rep_len(FALSE, length(x))
}

#' @export
geo_has_z.geovctrs_rect <- function(x) {
  rep_len(FALSE, length(x))
}

#' @rdname geo_set_z
#' @export
geo_z_range <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_z_range")
}

#' @export
geo_z_range.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_z_range(as_geovctr(x), ..., na.rm = na.rm, finite = finite)
}

#' @export
geo_z_range.wk_wkb <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- wkutils::wkb_ranges(x, na.rm, finite)
  new_geovctrs_lim(list(lower = ranges$zmin, upper = ranges$zmax))
}

#' @export
geo_z_range.wk_wkt <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- wkutils::wkt_ranges(x, na.rm, finite)
  new_geovctrs_lim(list(lower = ranges$zmin, upper = ranges$zmax))
}

#' @export
geo_z_range.geovctrs_xy <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_z_range(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @export
geo_z_range.geovctrs_segment <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_z_range(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @export
geo_z_range.geovctrs_rect <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_z_range(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @rdname geo_set_z
#' @export
geo_z_envelope <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_z_envelope")
}

#' @export
geo_z_envelope.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_z_envelope(as_geovctr(x), ..., na.rm = na.rm, finite = finite)
}

#' @export
geo_z_envelope.wk_wkt <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- wkutils::wkt_feature_ranges(x, na.rm, finite)
  new_geovctrs_lim(list(lower = ranges$zmin, upper = ranges$zmax))
}

#' @export
geo_z_envelope.wk_wkb <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- wkutils::wkb_feature_ranges(x, na.rm, finite)
  new_geovctrs_lim(list(lower = ranges$zmin, upper = ranges$zmax))
}

#' @export
geo_z_envelope.wk_wkb <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  ranges <- wkutils::wksxp_feature_ranges(x, na.rm, finite)
  new_geovctrs_lim(list(lower = ranges$zmin, upper = ranges$zmax))
}

#' @export
geo_z_envelope.geovctrs_xy <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_z_envelope(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @export
geo_z_envelope.geovctrs_segment <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_z_envelope(as_wksxp(x), na.rm = na.rm, finite = finite)
}

#' @export
geo_z_envelope.geovctrs_rect <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_z_envelope(as_wksxp(x), na.rm = na.rm, finite = finite)
}
