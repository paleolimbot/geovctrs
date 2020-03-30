
#' Convert a geometry to a different data structure
#'
#' @param x A geometry-like object
#' @param to A prototype
#'
#' @return A geometry vector, in the format defined by `to`.
#' @noRd
#'
#' @examples
#' geo_convert(geo_wkt("POINT (20 10)"), geo_wkb())
#'
geo_convert <- function(x, to) {
  cpp_convert(x, to)
}
