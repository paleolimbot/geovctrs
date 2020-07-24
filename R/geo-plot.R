
#' Plot geometry vectors
#'
#' These functions are basic plot implementations for the geovctrs types.
#' Similar [plot()] methods are available for [well-known types][wkutils::wkt_plot]
#' in the wkutils package.
#'
#' @inheritParams geo_bbox
#' @param ... Passed to plotting functions for features: [graphics::points()]
#'   for xy vectors, [graphics::segments()] for segment vectors, and
#'   [graphics::rect()] for rectangles.
#' @param bbox The limits of the plot. Defaults to `geo_bbox(x, finite = TRUE)`.
#' @param asp,xlab,ylab Passed to [graphics::plot()]
#' @param add Should the object be added the current plot?
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
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
