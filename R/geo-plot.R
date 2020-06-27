
#' Plot geometry vectors
#'
#' These functions are basic plot implementations for the geovctrs types.
#' Similar [plot()] methods are available for [well-known types][wk::plot.wk_wkt]
#' in the wk package.
#'
#' @inheritParams geo_bbox
#' @param ... Passed to plotting functions for features: [graphics::points()]
#'   for xy vectors, [graphics::segments()] for segment vectors, and
#'   [graphics::rect()] for rectangles.
#' @param bbox The limits of the plot. Defaults to `geo_bbox(x, finite = TRUE)`.
#' @param asp,xlab,ylab Passed to [graphics::plot()]
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
#' # plot functions for wk types are provided
#' # in the wk package:
#' plot(wkt("POINT (10 40)"))
#' plot(wkt("LINESTRING (30 10, 10 30, 40 40)"))
#' plot(wkt("MULTIPOINT ((10 40), (40 30))"))
#' plot(wkt("MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))"))
#' plot(wkt("POLYGON ((30 10, 10 30, 40 40, 30 10))"))
#' plot(
#'   wkt(
#'     "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),
#'       ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))"
#'   ),
#'   col = "grey90"
#' )
#'
#' # plot functions with the same arguments are also available:
#' plot(geo_xy(1:5, 1:5))
#' plot(geo_segment(0, 0, 1, 1))
#' plot(geo_rect(0, 1, 2, 3), col = "grey90")
#'
plot.geovctrs_xy <- function(x, ..., add = FALSE, asp = 1,
                             bbox = geo_bbox(x, finite = TRUE), xlab = "", ylab = "") {
  maybe_create_plot(x, add = add, asp = asp, bbox = bbox, xlab = xlab, ylab = ylab)
  graphics::points(field(x, "x"), field(x, "y"), ...)
  invisible(x)
}

#' @rdname plot.geovctrs_xy
#' @export
plot.geovctrs_segment <- function(x, ..., add = FALSE, asp = 1,
                                  bbox = geo_bbox(x, finite = TRUE), xlab = "", ylab = "") {
  maybe_create_plot(x, add = add, asp = asp, bbox = bbox, xlab = xlab, ylab = ylab)
  graphics::segments(
    field(x, "x0"), field(x, "y0"),
    field(x, "x1"), field(x, "y1"),
    ...
  )

  invisible(x)
}

#' @rdname plot.geovctrs_xy
#' @export
plot.geovctrs_rect <- function(x, ..., add = FALSE, asp = 1,
                               bbox = geo_bbox(x, finite = TRUE), xlab = "", ylab = "") {
  maybe_create_plot(x, add = add, asp = asp, bbox = bbox, xlab = xlab, ylab = ylab)
  graphics::rect(
    field(x, "xmin"), field(x, "ymin"),
    field(x, "xmax"), field(x, "ymax"),
    ...
  )

  invisible(x)
}

maybe_create_plot <- function(x, add = FALSE, asp = 1,
                              bbox = geo_bbox(x, finite = TRUE), xlab = "", ylab = "") {
  if (!add) {
    bbox <- as_geo_rect(bbox)
    xlim <- c(field(bbox, "xmin"), field(bbox, "xmax"))
    ylim <- c(field(bbox, "ymin"), field(bbox, "ymax"))

    graphics::plot(
      numeric(0),
      numeric(0),
      xlim = xlim,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      asp = asp
    )
  }
}
