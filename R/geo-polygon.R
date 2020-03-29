
#' Create a polygon
#'
#' @inheritParams geo_point
#'
#' @return A [geo_collection()] of length 1.
#' @export
#'
#' @examples
#' geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10)))
#'
geo_polygon <- function(xy, ring = 1L, srid = NA)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  feat <- new_geo_polygon(as_part_identifier(xy, ring = ring))
  validate_geo_polygon(feat)
  new_geo_collection(list(feature = list(feat), srid = srid))
}

new_geo_polygon <- function(x) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$ring, integer())
  structure(x, class = "geo_polygon")
}

validate_geo_polygon <-function(x) {
  stopifnot(!(vec_size(x$xy) %in% c(1, 2)))
  invisible(x)
}
