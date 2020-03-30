
#' Bounding boxes
#'
#' @param x A geometry-like object
#' @param ... Unused
#' @param na.rm Should NAs be considered?
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
geo_bbox.default <- function(x, ..., na.rm = FALSE) {
  rects <- geo_convert(x, geo_rect())
  xs <- c(field(rects, "xmin"), field(rects, "xmax"))
  ys <- c(field(rects, "ymin"), field(rects, "ymax"))
  xlim <- range(xs[is.finite(xs)], na.rm = na.rm)
  ylim <- range(ys[is.finite(ys)], na.rm = na.rm)
  geo_rect(xlim[1], ylim[1], xlim[2], ylim[2])
}

#' @rdname geo_bbox
#' @export
geo_bbox.geo_collection <- function(x, ..., na.rm = FALSE) {
  rects <- vapply(field(x, "feature"), function(f) {
    xy <- field(f, "xy")
    xlim <- range(field(xy, "x"), na.rm = na.rm)
    ylim <- range(field(xy, "y"), na.rm = na.rm)
    c(xlim[1], ylim[1], xlim[2], ylim[2])
  }, numeric(4))

  xlim <- range(c(rects[1, ], rects[3, ]), na.rm = na.rm)
  ylim <- range(c(rects[2, ], rects[4, ]), na.rm = na.rm)
  geo_rect(xlim[1], ylim[1], xlim[2], ylim[2])
}
