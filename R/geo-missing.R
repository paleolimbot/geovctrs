
#' Missing and non-finite geometries
#'
#' @inheritParams geo_bbox
#'
#' @return A logical vector
#' @export
#'
geo_is_missing <- function(x) {
  UseMethod("geo_is_missing")
}

#' @export
geo_is_missing.default <- function(x) {
  is.na(as_geovctr(x))
}

#' @rdname geo_is_missing
#' @export
geo_has_missing <- function(x) {
  UseMethod("geo_has_missing")
}

#' @export
geo_has_missing.default <- function(x) {
  geo_has_missing(as_geovctr(x))
}

#' @export
geo_has_missing.geovctr <- function(x) {
  cpp_has_missing(x)
}

#' @export
geo_has_missing.geo_wkt <- function(x) {
  grepl("nan", vec_data(x))
}

#' @export
geo_has_missing.geo_xy <- function(x) {
  is.na(field(x, "x")) | is.na(field(x, "y"))
}

#' @export
geo_has_missing.geo_segment <- function(x) {
  start <- field(x, "start")
  end <- field(x, "end")
  is.na(field(start, "x")) |
    is.na(field(start, "y")) |
    is.na(field(end, "x")) |
    is.na(field(end, "y"))
}

#' @export
geo_has_missing.geo_rect <- function(x) {
  is.na(field(x, "xmin")) |
    is.na(field(x, "ymin")) |
    is.na(field(x, "xmax")) |
    is.na(field(x, "ymax"))
}

#' @rdname geo_is_missing
#' @export
geo_is_finite <- function(x) {
  UseMethod("geo_is_finite")
}

#' @export
geo_is_finite.default <- function(x) {
  geo_is_finite(as_geovctr(x))
}

#' @export
geo_is_finite.geovctr <- function(x) {
  cpp_is_finite(x)
}

#' @export
geo_is_finite.geo_wkt <- function(x) {
  !grepl("inf|nan", vec_data(x))
}

#' @export
geo_is_finite.geo_xy <- function(x) {
  is.finite(field(x, "x")) &
    is.finite(field(x, "y"))
}

#' @export
geo_is_finite.geo_segment <- function(x) {
  start <- field(x, "start")
  end <- field(x, "end")
  is.finite(field(start, "x")) &
    is.finite(field(start, "y")) &
    is.finite(field(end, "x")) &
    is.finite(field(end, "y"))
}

#' @export
geo_is_finite.geo_rect <- function(x) {
  is.finite(field(x, "xmin")) &
    is.finite(field(x, "ymin")) &
    is.finite(field(x, "xmax")) &
    is.finite(field(x, "ymax"))
}
