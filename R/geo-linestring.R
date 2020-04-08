
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
geo_linestring <- function(xy, srid = 0)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  feat <- new_geovctrs_linestring(list(xy = xy))
  validate_geo_linestring(feat)
  new_geovctrs_collection(list(feature = list(feat), srid = as_geo_srid(srid)))
}

new_geovctrs_linestring <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_linestring")
}

validate_geo_linestring <-function(x) {
  stopifnot(vec_size(x$xy) != 1)
  invisible(x)
}
