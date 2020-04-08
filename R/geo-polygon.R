
#' Create a polygon
#'
#' @inheritParams geo_point
#' @param ring A vector whose unique value identifies rings.
#'   These values are considered in order, and the resulting coordinates
#'   are reordered such that rings are grouped together.
#'
#' @return A [geo_collection()] of length 1.
#' @export
#'
#' @examples
#' # a polygon
#' geo_polygon(geo_xy(c(0, 10, 0, 0), c(0, 0, 10, 0)))
#'
#' # polygon with a hole
#' poly_hole <- geo_polygon(
#'   geo_xy(
#'     c(35, 45, 15, 10, 35, 20, 35, 30, 20),
#'     c(10, 45, 40, 20, 10, 30, 35, 20, 30)
#'   ),
#'   ring = c(1, 1, 1, 1, 1, 2, 2, 2, 2)
#' )
#'
geo_polygon <- function(xy, ring = 1L, srid = 0)  {
  xy <- vec_cast(xy, geo_xy())
  stopifnot(vec_size(srid) == 1)

  feat <- new_geovctrs_polygon(as_part_identifier(xy, ring = ring))
  validate_geo_polygon(feat)
  new_geovctrs_collection(list(feature = list(feat), srid = as_geo_srid(srid)))
}

new_geovctrs_polygon <- function(x) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$ring, integer())
  structure(x, class = "geo_polygon")
}

validate_geo_polygon <-function(x) {
  rings <- split(x$xy, x$ring)
  lengths <- vapply(rings, length, integer(1))
  stopifnot(!any(lengths %in% c(1, 2)))

  invisible(x)
}
