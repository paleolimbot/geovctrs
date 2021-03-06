
#' Missing, empty, and non-finite geometries
#'
#' Whereas EMPTY geometries are a concept at the geometry level,
#' missing values are a concept of the in-memory vectors used to store
#' them in R. The [geo_is_missing()] function can be used to identify
#' these values, similar to [is.na()]. Missing coordinates (`NA` or `NaN`)
#' can be identified using [geo_has_missing()], and [geo_is_finite()]
#' can be used to ensure that all coordinates are finite. Note that
#' `geo_xy(NA, NA)`, `wkt("POINT (nan nan)")`, and
#' `wkt("MULTIPOINT (nan nan)")` are all considered empty points,
#' and are therefore non-misssing, contain no missing coordinates, and
#' are finite (use `is.na()` and/or [stringr::str_detect()]) if you
#' would like to specifically detect these cases).
#'
#' @inheritParams geo_bbox
#'
#' @return A logical vector
#' @format NULL
#' @export
#'
#' @examples
#' geo_is_missing(NA_wkt_)
#' geo_has_missing(NA_wkt_)
#' geo_is_finite(NA_wkt_)
#' geo_is_empty(NA_wkt_)
#'
#' geo_is_missing(wkt("LINESTRING (10 inf, nan 2)"))
#' geo_has_missing(wkt("LINESTRING (10 inf, nan 2)"))
#' geo_is_finite(wkt("LINESTRING (10 inf, nan 2)"))
#' geo_is_empty(wkt("LINESTRING (10 inf, nan 2)"))
#'
#' geo_is_missing(wkt("LINESTRING (10 inf, 1 2)"))
#' geo_has_missing(wkt("LINESTRING (10 inf, 1 2)"))
#' geo_is_finite(wkt("LINESTRING (10 inf, 1 2)"))
#' geo_is_empty(wkt("LINESTRING (10 inf, 1 2)"))
#'
#' # EMPTY geometries are considered finite and non-missing
#' geo_is_missing(wkt("LINESTRING EMPTY"))
#' geo_has_missing(wkt("LINESTRING EMPTY"))
#' geo_is_finite(wkt("LINESTRING EMPTY"))
#' geo_is_empty(wkt("LINESTRING EMPTY"))
#'
#' # POINT EMPTY, POINT (nan nan), and geo_xy(NA, NA)
#' # are all empty points
#' geo_is_missing(wkt("POINT EMPTY"))
#' geo_has_missing(wkt("POINT EMPTY"))
#' geo_is_finite(wkt("POINT EMPTY"))
#' geo_is_empty(wkt("POINT EMPTY"))
#'
#' geo_is_missing(wkt("POINT (nan nan)"))
#' geo_has_missing(wkt("POINT (nan nan)"))
#' geo_is_finite(wkt("POINT (nan nan)"))
#' geo_is_empty(wkt("POINT (nan nan)"))
#'
#' geo_is_missing(geo_xy(NA, NA))
#' geo_has_missing(geo_xy(NA, NA))
#' geo_is_finite(geo_xy(NA, NA))
#' geo_is_empty(geo_xy(NA, NA))
#'
geo_is_missing <- function(x) {
  UseMethod("geo_is_missing")
}

#' @export
geo_is_missing.default <- function(x) {
  is.na(x)
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
geo_has_missing.wk_wkt <- function(x) {
  wkutils::wkt_has_missing(x)
}

#' @export
geo_has_missing.wk_wkb <- function(x) {
  wkutils::wkb_has_missing(x)
}

#' @export
geo_has_missing.wk_wksxp <- function(x) {
  wkutils::wksxp_has_missing(x)
}

#' @export
geo_has_missing.geovctrs_xy <- function(x) {
  (is.na(field(x, "x")) | is.na(field(x, "y")))
}

#' @export
geo_has_missing.geovctrs_xyz <- function(x) {
  (is.na(field(x, "x")) | is.na(field(x, "y")) | is.na(field(x, "z")))
}

#' @export
geo_has_missing.geovctrs_segment <- function(x) {
  result <- is.na(field(x, "x0")) |
    is.na(field(x, "y0")) |
    is.na(field(x, "x1")) |
    is.na(field(x, "y1"))
  result[is.na(x)] <- NA
  result
}

#' @export
geo_has_missing.geovctrs_rect <- function(x) {
  result <- is.na(field(x, "xmin")) |
    is.na(field(x, "ymin")) |
    is.na(field(x, "xmax")) |
    is.na(field(x, "ymax"))
  result[is.na(x)] <- NA
  result
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
geo_is_finite.wk_wkt <- function(x) {
  wkutils::wkt_is_finite(x)
}

#' @export
geo_is_finite.wk_wkb <- function(x) {
  wkutils::wkb_is_finite(x)
}

#' @export
geo_is_finite.wk_wksxp <- function(x) {
  wkutils::wksxp_is_finite(x)
}

#' @export
geo_is_finite.geovctrs_xy <- function(x) {
  is.finite(field(x, "x")) &
    is.finite(field(x, "y"))
}

#' @export
geo_is_finite.geovctrs_xyz <- function(x) {
  is.finite(field(x, "x")) &
    is.finite(field(x, "y")) &
    is.finite(field(x, "z"))
}

#' @export
geo_is_finite.geovctrs_segment <- function(x) {
  result <- is.finite(field(x, "x0")) &
    is.finite(field(x, "y0")) &
    is.finite(field(x, "x1")) &
    is.finite(field(x, "y1"))
  result[is.na(x)] <- NA
  result
}

#' @export
geo_is_finite.geovctrs_rect <- function(x) {
  result <- is.finite(field(x, "xmin")) &
    is.finite(field(x, "ymin")) &
    is.finite(field(x, "xmax")) &
    is.finite(field(x, "ymax"))
  result[is.na(x)] <- NA
  result
}

#' @rdname geo_is_missing
#' @export
geo_is_empty <- function(x) {
  UseMethod("geo_is_empty")
}

#' @export
geo_is_empty.default <- function(x) {
  geo_is_empty(as_geovctr(x))
}

#' @export
geo_is_empty.wk_wkt <- function(x) {
  is.na(x) | wkutils::wkt_meta(x)$size == 0
}

#' @export
geo_is_empty.wk_wkb <- function(x) {
  is.na(x) | wkutils::wkb_meta(x)$size == 0
}

#' @export
geo_is_empty.wk_wksxp <- function(x) {
  is.na(x) | wkutils::wksxp_meta(x)$size == 0
}

#' @export
geo_is_empty.geovctrs_xy <- function(x) {
  is.na(x)
}

#' @export
geo_is_empty.geovctrs_xyz <- function(x) {
  is.na(x)
}

#' @export
geo_is_empty.geovctrs_segment <- function(x) {
  x <- vec_data(x)
  is.na(x$x0) & is.na(x$y0) & is.na(x$x1) & is.na(x$y1)
}

#' @export
geo_is_empty.geovctrs_rect <- function(x) {
  # rules are more complex for rect because of Inf, -Inf
  geo_is_empty(as_wksxp(x))
}

# ----- missing values (assigned in .onLoad) --------

#' @rdname geo_is_missing
#' @export
NA_wkt_ <- NULL

#' @rdname geo_is_missing
#' @export
NA_wkb_ <- NULL

#' @rdname geo_is_missing
#' @export
NA_collection_ <- NULL

#' @rdname geo_is_missing
#' @export
NA_xy_ <- NULL

#' @rdname geo_is_missing
#' @export
NA_segment_ <- NULL

#' @rdname geo_is_missing
#' @export
NA_rect_ <- NULL
