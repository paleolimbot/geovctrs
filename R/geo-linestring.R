
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

  feat <- new_geo_linestring(list(xy = xy))
  validate_geo_linestring(feat)
  new_geo_collection(list(feature = list(feat), srid = vec_cast(srid, integer())))
}

new_geo_linestring <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_linestring")
}

validate_geo_linestring <-function(x) {
  stopifnot(vec_size(x$xy) != 1)
  invisible(x)
}

#' @export
format.geo_linestring <- function(x, ...) {
  format.geo_point(
    x,
    ...,
    non_empty = {
      first_last <- format(c(x$xy[1], x$xy[length(x$xy)]))
      sprintf("%s...{%s}...%s", first_last[1], length(x$xy) - 2, first_last[2])
    }
  )
}
