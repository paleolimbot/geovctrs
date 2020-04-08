
#' Create a multipoint
#'
#' @inheritParams geo_point
#' @param feature A [geo_collection()] of [geo_point()]s.
#'
#' @return A [geo_collection()] of length 1.
#' @export
#'
#' @examples
#' geo_multipoint(
#'   geo_point(geo_xy(10, 30))
#' )
#'
geo_multipoint <- function(feature, srid = 0) {
  feature <- vec_cast(feature, geo_collection())
  values <- field(feature, "feature")
  is_point <- vapply(values, inherits, "geo_point", FUN.VALUE = logical(1))
  if (!all(is_point)) {
    abort("All features must be `geo_point()`s")
  }

  xy <- lapply(values, `[[`, "xy")

  feat <- new_geovctrs_multipoint(
    list(
      xy = vec_c(!!!xy, .ptype = geo_xy())
    )
  )

  new_geovctrs_collection(list(feature = list(feat), srid = as_geo_srid(srid)))
}

new_geovctrs_multipoint <- function(x) {
  vec_assert(x$xy, geo_xy())
  structure(x, class = "geo_multipoint")
}
