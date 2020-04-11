
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
#' @return A [geo_rect()] of length 1.
#' @export
#'
#' @examples
#' geo_bbox(geo_wkt(c("POINT (30 10)", "POINT EMPTY")))
#' geo_envelope(geo_wkt(c("POINT (30 10)", "POINT EMPTY")))
#'
geo_bbox <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_bbox")
}

#' @export
geo_bbox.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geo_bbox(as_geovctr(x), ..., na.rm = na.rm, finite = finite)
}

#' @export
geo_bbox.geovctr <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geovctrs_cpp_bbox(x, na.rm, finite)
}

#' @rdname geo_bbox
#' @export
geo_envelope <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_envelope")
}

#' @export
geo_envelope.default <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  restore_geovctr(x, geovctrs_cpp_envelope(as_geovctr(x), na.rm, finite))
}

#' @export
geo_envelope.geovctr <- function(x, ..., na.rm = FALSE, finite = FALSE) {
  geovctrs_cpp_envelope(x, na.rm, finite)
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

  if (na.rm) {
    xmin[is.na(xs)] <- Inf
    ymin[is.na(ys)] <- Inf
    xmax[is.na(xs)] <- -Inf
    ymax[is.na(ys)] <- -Inf
  }

  geo_rect(xmin, ymin, xmax, ymax)
}
