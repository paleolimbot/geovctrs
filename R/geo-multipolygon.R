
#' Create a multipolygon
#'
#' @inheritParams geo_point
#' @param feature A [geo_collection()] of [geo_polygon()]s.
#'
#' @return A [geo_collection()] of length 1.
#' @export
#'
#' @examples
#' geo_multipolygon(
#'   geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10)))
#' )
#'
geo_multipolygon <- function(feature, srid = 0) {
  feature <- vec_cast(feature, geo_collection())
  values <- field(feature, "feature")
  is_polygon <- vapply(values, inherits, "geo_polygon", FUN.VALUE = logical(1))
  if (!all(is_polygon)) {
    abort("All features must be `geo_polygon()`s")
  }

  xy <- lapply(values, `[[`, "xy")
  ring <- lapply(values, `[[`, "ring")
  lengths <- vapply(xy, length, integer(1))
  part_rle <- structure(list(lengths = lengths, values = seq_along(lengths)), class = "rle")

  feat <- new_geo_multipolygon(
    list(
      xy = vec_c(!!!xy, .ptype = geo_xy()),
      part = inverse.rle(part_rle),
      ring = vec_c(!!!ring, .ptype = integer())
    )
  )

  new_geo_collection(list(feature = list(feat), srid = vec_cast(srid, integer())))
}

new_geo_multipolygon <- function(x) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$part, integer())
  vec_assert(x$ring, integer())
  structure(x, class = "geo_multipolygon")
}

validate_geo_multipolygon <-function(x) {
  invisible(x)
}

#' @export
format.geo_multipolygon <- function(x, ...) {
  format.geo_linestring(x, ...)
}
