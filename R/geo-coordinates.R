
#' Extract coordinates as a tibble
#'
#' @inheritParams geo_bbox
#'
#' @return A tibble with one row per coordinate and columns
#'   `feature` and `xy`. Vectors that include mutli geometries
#'   will have a `part` column, and vectors that include polygons
#'   will have a `ring` column.
#' @export
#'
#' @examples
#' geo_coordinates("POINT (20 17)")
#' geo_coordinates("POINT EMPTY")
#'
#' geo_coordinates("LINESTRING EMPTY")
#' geo_coordinates("LINESTRING (30 10, 0 0)")
#'
#' geo_coordinates("POLYGON EMPTY")
#' geo_coordinates("POLYGON ((30 10, 0 0, -30 10, 30 10))")
#'
#' geo_coordinates(
#'   "MULTIPOLYGON (
#'     ((40 40, 20 45, 45 30, 40 40)),
#'     ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),
#'       (30 20, 20 15, 20 25, 30 20))
#'   )"
#' )
#'
geo_coordinates <- function(x, ...) {
  UseMethod("geo_coordinates")
}

#' @export
geo_coordinates.default <- function(x, ...) {
  geo_coordinates(as_geovctr(x), ...)
}

#' @export
geo_coordinates.geovctr <- function(x, ...) {
  geo_coordinates(as_geo_collection(x), ...)
}

#' @export
geo_coordinates.wk_wkt <- function(x, ...) {
  coords <- wk::wkt_coords(x, sep_na = FALSE)
  geo_coordinates_from_coords(coords)
}

#' @export
geo_coordinates.wk_wkb <- function(x, ...) {
  coords <- wk::wkb_coords(x, sep_na = FALSE)
  geo_coordinates_from_coords(coords)
}

#' @export
geo_coordinates.wk_wksxp <- function(x, ...) {
  coords <- wk::wksxp_coords(x, sep_na = FALSE)
  geo_coordinates_from_coords(coords)
}

geo_coordinates_from_coords <- function(coords) {
  has_z <- !anyNA(coords$z) && nrow(coords) > 0
  has_m <- !anyNA(coords$m) && nrow(coords) > 0
  if (has_m) {
    abort("geovctrs doesn't support the 'm' coordinate (yet)")
  }

  if (has_z) {
    xy <- geo_xyz(coords$x, coords$y, coords$z)
  } else {
    xy <- geo_xy(coords$x, coords$y)
  }

  tibble::new_tibble(
    list(
      feature = coords$feature,
      part = coords$part,
      ring = coords$ring,
      xy = xy
    ),
    nrow = nrow(coords)
  )
}

#' @export
geo_coordinates.geovctrs_collection <- function(x, ...) {
  geo_coordinates(geovctrs_cpp_convert(x, wkb()), ...)
}
