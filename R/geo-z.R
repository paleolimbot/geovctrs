
#' Work with Z values
#'
#' Currently, geovctrs supports X, Y, and Z coordinates. These functions help extract,
#' set, and drop Z values.
#'
#' @inheritParams geo_bbox
#' @param z A vector of z coordinate values.
#'
#' @return A modified version of `x`.
#' @export
#'
#' @examples
#' geo_set_z("POINT (2 3)", 4)
#' geo_drop_z("POINT Z (2 3 4)")
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
geo_set_z.geovctr <- function(x, z) {
  params <- recycle_parameter(x, z = vec_cast(z, double()))
  geovctrs_cpp_set_z(x, params$z)
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

#' @rdname geo_set_z
#' @export
geo_has_z <- function(x) {
  UseMethod("geo_has_z")
}

#' @export
geo_has_z.default <- function(x) {
  geo_summary(x)$has_z
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
geo_z_range.geovctr <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geovctrs_cpp_z_range(x, na.rm, finite)
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
geo_z_envelope.geovctr <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geovctrs_cpp_z_envelope(x, na.rm, finite)
}
