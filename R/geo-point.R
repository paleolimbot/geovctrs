
#' Create a point
#'
#' @param xy A [geo_xy()]
#' @param srid A spatial reference identifier.
#'
#' @return A [geo_collection()] of length 1.
#' @export
#'
#' @examples
#' geo_point(geo_xy(0, 0))
#'
geo_point <- function(xy, srid = NA)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  point <- new_geo_point(list(xy = xy))
  validate_geo_point(point)
  new_geo_collection(list(feature = list(point), srid = srid))
}

new_geo_point <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_point")
}

validate_geo_point <-function(x) {
  stopifnot(vec_size(x$xy) %in% c(0, 1))
  invisible(x)
}

#' @export
format.geo_point <- function(x, ..., non_empty = format(x$xy[1])) {
  if (length(x$xy) == 0) {
    sprintf("<%s empty>", class(x)[1])
  } else {
    sprintf("<%s %s>", class(x)[1], non_empty)
  }
}
