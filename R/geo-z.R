
#' Work with Z values
#'
#' Currently, geovctrs supports X, Y, and Z coordinates. These functions help extract,
#' set, and drop Z values.
#'
#' @inheritParams geo_bbox
#' @param z A z value
#'
#' @return A modified version of `x`.
#' @export
#'
#' @examples
#' geo_set_z("POINT (2 3)", 4)
#' geo_drop_z("POINT Z (2 3 4)")
#'
geo_set_z <- function(x, z) {
  UseMethod("geo_set_z")
}

#' @export
geo_set_z.default <- function(x, z) {
  restore_geovctr(x, geo_set_z(as_geovctr(x), z))
}

#' @export
geo_set_z.geovctr <- function(x, z) {
  geovctrs_cpp_set_z(x, vec_recycle(vec_cast(z, double()), vec_size(x)))
}

#' @rdname geo_set_z
#' @export
geo_drop_z <- function(x) {
  UseMethod("geo_drop_z")
}

#' @export
geo_drop_z.default <- function(x) {
  restore_geovctr(x, geo_drop_z(as_geovctr(x)))
}

#' @export
geo_drop_z.geovctr <- function(x) {
  geovctrs_cpp_drop_z(x, x)
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

