
#' Plot geometry vectors
#'
#' Currently, only geometries of the same type can be plotted in the same
#' plot call (geometries are promoted to mutli-geometries if needed).
#'
#' @inheritParams geo_bbox
#' @param ... Passed to plotting functions for features: [graphics::points()]
#'   for point and multipoint geometries, [graphics::lines()] for linestring
#'   and multilinestring geometries, and [graphics::polypath()] for polygon
#'   and multipolygon geometries.
#' @param bbox The limits of the plot. Defaults to `geo_bbox(x, finite = TRUE)`.
#' @param asp,xlab,ylab Passed to [graphics::plot()]
#' @param rule The rule to use for filling polygons (see [graphics::polypath()])
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
#' geo_plot(wkt("POINT (10 40)"))
#' geo_plot(wkt("LINESTRING (30 10, 10 30, 40 40)"))
#' geo_plot(wkt("MULTIPOINT ((10 40), (40 30))"))
#' geo_plot(wkt("MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))"))
#' geo_plot(wkt("POLYGON ((30 10, 10 30, 40 40, 30 10))"))
#' geo_plot(
#'   wkt(
#'     "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),
#'       ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))"
#'   ),
#'   col = "grey90"
#' )
#'
#' # can also plot data frames that have exactly one geovctr column
#' prev_pal <- palette(grey.colors(10))
#' geo_plot(geo_nc, col = cut(BIR79, 10))
#'
#' # restore initial palette
#' palette(prev_pal)
#'
geo_plot <- function(x, ..., asp = 1, bbox = geo_bbox(x, finite = TRUE), xlab = "", ylab = "") {
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

  geo_plot_add(x, ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add <- function(x, ...) {
  UseMethod("geo_plot_add")
}

#' @rdname geo_plot
#' @export
geo_plot_add.default <- function(x, ...) {
  geo_plot_add(as_geovctr(x), ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geovctr <- function(x, ...) {
  geo_plot_add(as_wksxp(x), ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.data.frame <- function(x, ...) {
  # evaluate the dots
  dots <- rlang::quos(...)
  dots_eval <- lapply(dots, rlang::eval_tidy, data = x)
  dots_tbl <- vec_recycle_common(!!!dots_eval, .size = nrow(x))

  # plot the features
  do.call(geo_plot_add, c(list(as_geovctr(x)), dots_tbl))

  # return the input
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.NULL <- function(x, ...) {
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geovctrs_xy <- function(x, ...) {
  graphics::points(field(x, "x"), field(x, "y"), ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geovctrs_segment <- function(x, ...) {
  graphics::segments(
    field(x, "x0"), field(x, "y0"),
    field(x, "x1"), field(x, "y1"),
    ...
  )

  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geovctrs_rect <- function(x, ...) {
  graphics::rect(
    field(x, "xmin"), field(x, "ymin"),
    field(x, "xmax"), field(x, "ymax"),
    ...
  )

  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.wk_wksxp <- function(x, ..., rule = "evenodd") {
  # evaluate dots, wrap scalar types in a list(), and vectorize
  dots <- rlang::list2(..., rule = rule)
  is_scalar <- !vapply(dots, vec_is, logical(1))
  dots[is_scalar] <- lapply(dots[is_scalar], list)
  dots_tbl <- vec_recycle_common(!!!dots, .size = length(x))
  meta <- unclass(wk::wksxp_meta(x, recursive = FALSE))

  # using for() because the user interrupt is respected in RStudio
  for (i in seq_along(x)) {
    coords <- wk::wksxp_coords(x[[i]], sep_na = TRUE)[c("x", "y")]
    if (nrow(coords) == 0) {
      next
    }

    dots_item <- lapply(dots_tbl, "[[", i)
    type_id <- meta$type[i]
    args <- c(coords, dots_item)

    if (type_id == 1 || type_id == 4) {
      args$rule <- NULL
      do.call(graphics::points, args)
    } else if (type_id == 2 || type_id == 5) {
      args$rule <- NULL
      do.call(graphics::lines, args)
    } else if (type_id == 3 || type_id == 6) {
      do.call(graphics::polypath, args)
    } else if (type_id == 7) {
      do.call(geo_plot_add.wk_wksxp, c(list(wksxp(unclass(x)[[i]])), dots_item))
    } else {
      abort("Unknown geometry type") # nocov
    }
  }

  invisible(x)
}

#' @importFrom graphics plot
#' @export
plot.geovctr <- function(x, ...) {
  geo_plot(x, ...)
}
