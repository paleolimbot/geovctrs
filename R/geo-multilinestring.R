
#' Create a multilinestring
#'
#' @inheritParams geo_point
#' @param feature A [geo_collection()] of [geo_linestring()]s.
#'
#' @return A [geo_collection()] of length 1.
#' @export
#'
#' @examples
#' geo_multilinestring(
#'   geo_linestring(geo_xy(0:1, 0:1))
#' )
#'
geo_multilinestring <- function(feature, srid = 0) {
  feature <- vec_cast(feature, geo_collection())
  values <- field(feature, "feature")
  is_linestring <- vapply(values, inherits, "geo_linestring", FUN.VALUE = logical(1))
  if (!all(is_linestring)) {
    abort("All features must be `geo_linestring()`s")
  }

  xy <- lapply(values, `[[`, "xy")
  lengths <- vapply(xy, length, integer(1))
  part_rle <- structure(list(lengths = lengths, values = seq_along(lengths)), class = "rle")

  feat <- new_geo_multilinestring(
    list(
      xy = vec_c(!!!xy, .ptype = geo_xy()),
      part = inverse.rle(part_rle)
    )
  )

  new_geo_collection(list(feature = list(feat), srid = vec_cast(srid, integer())))
}

new_geo_multilinestring <- function(x) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$part, integer())
  structure(x, class = "geo_multilinestring")
}

validate_geo_multilinestring <-function(x) {
  invisible(x)
}

#' @export
format.geo_multilinestring <- function(x, ...) {
  format.geo_linestring(x, ...)
}
