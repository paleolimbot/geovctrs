
#' Create a point
#'
#' @param xy A [geo_xy()]
#' @inheritParams geo_srid
#'
#' @return A [geo_collection()] of length 1.
#' @export
#'
#' @examples
#' geo_point(geo_xy(0, 0))
#'
geo_point <- function(xy, srid = 0)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  point <- new_geovctrs_point(list(xy = xy))
  validate_geo_point(point)
  new_geovctrs_collection(list(feature = list(point), srid = as_geo_srid(srid)))
}

new_geovctrs_point <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_point")
}

validate_geo_point <-function(x) {
  stopifnot(vec_size(x$xy) %in% c(0, 1))
  invisible(x)
}
