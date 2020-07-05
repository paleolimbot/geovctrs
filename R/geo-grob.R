
#' Create graphical objects
#'
#' @inheritParams geo_bbox
#' @inheritParams wkutils::wkt_grob
#'
#' @return A [grid graphical object][grid::grob]
#' @export
#'
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(
#'   geo_grob("POINT (0.5 0.5)", col = "red", pch = 16, default.units = "npc")
#' )
#'
geo_grob <- function(x, ..., rule = "evenodd", default.units = "native", name = NULL, vp = NULL) {
  UseMethod("geo_grob")
}

#' @rdname geo_grob
#' @export
geo_grob.default <- function(x, ..., rule = "evenodd", default.units = "native", name = NULL, vp = NULL) {
  if (is_geovctr(x)) {
    geo_grob(as_wksxp(x), ..., rule = rule, default.units = default.units, name = name, vp = vp)
  } else {
    geo_grob(
      as_wksxp(as_geovctr(x)),
      ...,
      rule = rule,
      default.units = default.units,
      name = name,
      vp = vp
    )
  }
}

#' @rdname geo_grob
#' @export
geo_grob.wk_wkt <- function(x, ..., rule = "evenodd", default.units = "native", name = NULL, vp = NULL) {
  wkutils::wkt_grob(x, ..., rule = rule, default.units = default.units, name = name, vp = vp)
}

#' @rdname geo_grob
#' @export
geo_grob.wk_wkb <- function(x, ..., rule = "evenodd", default.units = "native", name = NULL, vp = NULL) {
  wkutils::wkb_grob(x, ..., rule = rule, default.units = default.units, name = name, vp = vp)
}

#' @rdname geo_grob
#' @export
geo_grob.wk_wksxp <- function(x, ..., rule = "evenodd", default.units = "native", name = NULL, vp = NULL) {
  wkutils::wksxp_grob(x, ..., rule = rule, default.units = default.units, name = name, vp = vp)
}

#' @rdname geo_grob
#' @export
geo_grob.geovctrs_xy <- function(x, ..., rule = "evenodd",
                                 default.units = "native", name = NULL, vp = NULL) {
  if (vec_size(x) == 0) {
    return(grid::gTree(name = name, vp = vp, children = grid::gList()))
  }

  gpar_values <- rlang::list2(...)
  pch <- gpar_values$pch %||% 1
  size <- gpar_values$size %||% grid::unit(1, "char")
  gpar_values$pch <- NULL
  gpar_values$size <- NULL

  grid::pointsGrob(
    field(x, "x"),
    field(x, "y"),
    pch = pch,
    size = size,
    gp = do.call(grid::gpar, gpar_values),
    default.units = default.units,
    name = name,
    vp = vp
  )
}

#' @rdname geo_grob
#' @export
geo_grob.geovctrs_segment <- function(x, ..., rule = "evenodd", default.units = "native",
                                      name = NULL, vp = NULL) {
  if (vec_size(x) == 0) {
    return(grid::gTree(name = name, vp = vp, children = grid::gList()))
  }

  gpar_values <- rlang::list2(...)
  arrow <- gpar_values$arrow
  gpar_values$arrow <- NULL

  grid::segmentsGrob(
    field(x, "x0"),
    field(x, "y0"),
    field(x, "x1"),
    field(x, "y1"),
    arrow = arrow,
    gp = do.call(grid::gpar, gpar_values),
    default.units = default.units,
    name = name,
    vp = vp
  )
}

#' @rdname geo_grob
#' @export
geo_grob.geovctrs_rect <- function(x, ..., rule = "evenodd", default.units = "native",
                                      name = NULL, vp = NULL) {
  if (vec_size(x) == 0) {
    return(grid::gTree(name = name, vp = vp, children = grid::gList()))
  }

  gpar_values <- rlang::list2(...)

  x <- vec_data(x)
  width <- x$xmax - x$xmin
  height <- x$ymax - x$ymin

  grid::rectGrob(
    x$xmin,
    x$xmax, width = width, height = height,
    hjust = 0, vjust = 0,
    gp = do.call(grid::gpar, gpar_values),
    default.units = default.units,
    name = name,
    vp = vp
  )
}
