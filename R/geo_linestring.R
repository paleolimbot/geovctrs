
#' Create a linestring
#'
#' @inheritParams geo_point
#'
#' @return A [geo_collection()] of length 1.
#' @export
#'
#' @examples
#' geo_linestring(geo_xy(1:5, 2:6))
#'
geo_linestring <- function(xy, srid = NA)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  point <- new_geo_linestring(list(xy = xy))
  validate_geo_linestring(point)
  new_geo_collection(list(feature = list(point), srid = srid))
}

new_geo_linestring <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_linestring")
}

validate_geo_linestring <-function(x) {
  stopifnot(vec_size(x$xy) != 1)
  invisible(x)
}
