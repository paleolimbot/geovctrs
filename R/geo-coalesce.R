
#' Find first non-empty feature
#'
#' Like [dplyr::coalesce()], [geo_coalesce()] returns the first non-missing
#' element (rowwise). It is useful for replacing empty and missing values
#' with a default.
#'
#' @param x,... Vectors that will be recycled to a common length. These don't
#'   necessarily need to be geovctrs, but do need methods for [geo_is_empty()]
#'   and must be castable to the type of the first argument.
#'
#' @return A vector with the type of the first argument.
#' @export
#'
#' @examples
#' wkt <- wkt(c("POINT (30 10)", "POINT EMPTY", NA))
#' geo_coalesce(wkt, wkt("POINT (0 0)"))
#'
geo_coalesce <- function(x, ...) {
  UseMethod("geo_coalesce")
}

#' @export
geo_coalesce.default <- function(x, ...) {
  restore_geovctr(x, geo_coalesce(as_geovctr(x), ...))
}

#' @export
geo_coalesce.wk_wkt <- function(x, ...) {
  geo_coalesce_default(x, ...)
}

#' @export
geo_coalesce.wk_wkb <- function(x, ...) {
  geo_coalesce_default(x, ...)
}

#' @export
geo_coalesce.wk_wksxp <- function(x, ...) {
  geo_coalesce_default(x, ...)
}

#' @export
geo_coalesce.geovctrs_xy <- function(x, ...) {
  geo_coalesce_default(x, ...)
}

#' @export
geo_coalesce.geovctrs_segment <- function(x, ...) {
  geo_coalesce_default(x, ...)
}

#' @export
geo_coalesce.geovctrs_rect <- function(x, ...) {
  geo_coalesce_default(x, ...)
}

geo_coalesce_default <- function(x, ...) {
  values <- vec_recycle_common(x, ...)

  out <- values[[1]]
  for (x in values[-1]) {
    out_empty <- geo_is_empty(out)
    out[out_empty] <- x[out_empty]
  }

  out
}
