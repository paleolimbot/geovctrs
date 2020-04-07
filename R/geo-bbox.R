
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
#'
#' @return A [geo_rect()] of length 1.
#' @export
#'
#' @examples
#' geo_bbox(geo_wkt(c("POINT (30 10)", "POINT EMPTY")))
#'
geo_bbox <- function(x, ..., na.rm = FALSE) {
  UseMethod("geo_bbox")
}

#' @rdname geo_bbox
#' @export
geo_envelope <- function(x, ..., na.rm = FALSE) {
  UseMethod("geo_envelope")
}

#' @export
geo_envelope.default <- function(x, ..., na.rm = FALSE) {
  restore_geovctr(x, cpp_envelope(as_geovctr(x), na.rm))
}

#' @export
geo_envelope.geo_xy <- function(x, ..., na.rm = FALSE) {
  xs <- field(x, "x")
  ys <- field(x, "y")
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

#' @export
geo_envelope.geo_segment <- function(x, ..., na.rm = FALSE) {
  # *almost* the same, except a geo_rect might not have been constructed
  # with bounds in the correct order
  start <- field(x, "start")
  end <- field(x, "end")
  x1 <- field(start, "x")
  y1 <- field(start, "y")
  x2 <- field(end, "x")
  y2 <- field(end, "y")

  if (na.rm) {
    xmin <- pmin2(x1, x2)
    ymin <- pmin2(y1, y2)
    xmax <- pmax2(x1, x2)
    ymax <- pmax2(y1, y2)
  } else {
    xmin <- pmin(x1, x2, na.rm = FALSE)
    ymin <- pmin(y1, y2, na.rm = FALSE)
    xmax <- pmax(x1, x2, na.rm = FALSE)
    ymax <- pmax(y1, y2, na.rm = FALSE)
  }

  geo_rect(xmin, ymin, xmax, ymax, srid = field(x, "srid"));
}

#' @export
geo_envelope.geo_rect <- function(x, ..., na.rm = FALSE) {
  # *almost* the same, except a geo_rect might not have been constructed
  # with bounds in the correct order
  xmin <- field(x, "xmin")
  ymin <- field(x, "ymin")
  xmax <- field(x, "xmax")
  ymax <- field(x, "ymax")

  if (na.rm) {
    xmin2 <- pmin2(xmin, xmax)
    ymin2 <- pmin2(ymin, ymax)
    xmax2 <- pmax2(xmin, xmax)
    ymax2 <- pmax2(ymin, ymax)
  } else {
    xmin2 <- pmin(xmin, xmax, na.rm = FALSE)
    ymin2 <- pmin(ymin, ymax, na.rm = FALSE)
    xmax2 <- pmax(xmin, xmax, na.rm = FALSE)
    ymax2 <- pmax(ymin, ymax, na.rm = FALSE)
  }

  geo_rect(xmin2, ymin2, xmax2, ymax2, srid = field(x, "srid"));
}

#' @export
geo_bbox.default <- function(x, ..., na.rm = FALSE) {
  rects <- geo_envelope(as_geovctr(x), ..., na.rm = na.rm)
  srid <- unique(geo_srid(rects))[1]

  xmin <- suppressWarnings(min(field(rects, "xmin"), na.rm = na.rm))
  ymin <- suppressWarnings(min(field(rects, "ymin"), na.rm = na.rm))
  xmax <- suppressWarnings(max(field(rects, "xmax"), na.rm = na.rm))
  ymax <- suppressWarnings(max(field(rects, "ymax"), na.rm = na.rm))

  geo_rect(xmin, ymin, xmax, ymax, srid = srid)
}
