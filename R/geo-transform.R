
#' Apply an affine transformation
#'
#' @inheritParams geo_bbox
#' @param trans A 3x3 transformation matrix describing an affine
#'   transformation of the input.
#'
#' @return A transformed `x`
#' @export
#'
#' @examples
#' geo_transform(
#'   "POINT (30 10)",
#'   # translation +12 +13
#'   matrix(c(1, 0, 0, 0, 1, 0, 12, 13, 1), ncol = 3)
#' )
#'
#' geo_transform(
#'   geo_nc,
#'   # translation +12 +13
#'   matrix(c(1, 0, 0, 0, 1, 0, 12, 13, 1), ncol = 3)
#' )[c("NAME", "geometry")]
#'
geo_transform <- function(x, trans, ...) {
  UseMethod("geo_transform")
}

#' @rdname geo_transform
#' @export
geo_transform.default <- function(x, trans, ...) {
  if (is_geovctr(x)) {
    geo_transform(as_wksxp(x), trans)
  } else {
    restore_geovctr(x, geo_transform(as_geovctr(x), trans))
  }
}

#' @rdname geo_transform
#' @export
geo_transform.wk_wkt <- function(x, trans, ...) {
  new_wk_wkt(wkutils::wkt_transform(x, as_trans_matrix(trans)))
}

#' @rdname geo_transform
#' @export
geo_transform.wk_wkb <- function(x, trans, ...) {
  new_wk_wkb(wkutils::wkb_transform(x, as_trans_matrix(trans)))
}

#' @rdname geo_transform
#' @export
geo_transform.wk_wksxp <- function(x, trans, ...) {
  new_wk_wksxp(wkutils::wksxp_transform(x, as_trans_matrix(trans)))
}

#' @rdname geo_transform
#' @export
geo_transform.geovctrs_xy <- function(x, trans, ...) {
  coord_matrix <- matrix(
    c(field(x, "x"), field(x, "y"), rep_len(1, length(x))),
    nrow = 3,
    byrow = TRUE
  )

  result <- as_trans_matrix(trans) %*% coord_matrix
  new_geovctrs_xy(list(x = result[1, , drop = TRUE], y = result[2, , drop = TRUE]))
}

#' @rdname geo_transform
#' @export
geo_transform.geovctrs_segment <- function(x, trans, ...) {
  start <- new_geovctrs_xy(list(x = field(x, "x0"), y = field(x, "y0")))
  end <- new_geovctrs_xy(list(x = field(x, "x1"), y = field(x, "y1")))
  new_start <- geo_transform(start, trans)
  new_end <- geo_transform(end, trans)
  new_geovctrs_segment(
    list(
      x0 = field(new_start, "x"), y0 = field(new_start, "y"),
      x1 = field(new_end, "x"), y1 = field(new_end, "y"),
      srid = field(x, "srid")
    )
  )
}

as_trans_matrix <- function(trans) {
  trans <- as.matrix(trans)
  stopifnot(ncol(trans) == 3, nrow(trans) == 3)
  trans
}
