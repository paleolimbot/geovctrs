
#' Create a multipoint
#'
#' @inheritParams geo_point
#'
#' @return A [geo_collection()] of length 1.
#' @export
#'
#' @examples
#' geo_multipoint(geo_xy(1:5, 2:6))
#'
geo_multipoint <- function(xy, srid = NA)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  point <- new_geo_multipoint(list(xy = xy))
  validate_geo_multipoint(point)
  new_geo_collection(list(feature = list(point), srid = srid))
}

new_geo_multipoint <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_multipoint")
}

validate_geo_multipoint <-function(x) {
  invisible(x)
}
