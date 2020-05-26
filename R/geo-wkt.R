
#' @importFrom wk as_wkt
#' @export
wk::as_wkt

#' @importFrom wk wkt
#' @export
wk::wkt

#' @importFrom wk parse_wkt
#' @export
wk::parse_wkt

#' @importFrom wk as_wkt
#' @export
as_wkt.geovctr <- function(x, ...) {
  vec_cast(x, new_wk_wkt())
}

#' @importFrom wk validate_wk_wkt
#' @importFrom wk vec_cast.wk_wkt
#' @method vec_cast.wk_wkt geovctrs_rect
#' @export
vec_cast.wk_wkt.geovctrs_rect <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkt geovctrs_segment
#' @export
vec_cast.wk_wkt.geovctrs_segment <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkt geovctrs_xy
#' @export
vec_cast.wk_wkt.geovctrs_xy <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkt geovctrs_xyz
#' @export
vec_cast.wk_wkt.geovctrs_xyz <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.wk_wkt geovctrs_collection
#' @export
vec_cast.wk_wkt.geovctrs_collection <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @importFrom wk vec_ptype2.wk_wkt
#' @method vec_ptype2.wk_wkt geovctrs_collection
#' @export
vec_ptype2.wk_wkt.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkt()
}

#' @method vec_ptype2.wk_wkt geovctrs_xy
#' @export
vec_ptype2.wk_wkt.geovctrs_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkt()
}

#' @method vec_ptype2.wk_wkt geovctrs_xyz
#' @export
vec_ptype2.wk_wkt.geovctrs_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkt()
}

#' @method vec_ptype2.wk_wkt geovctrs_segment
#' @export
vec_ptype2.wk_wkt.geovctrs_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkt()
}

#' @method vec_ptype2.wk_wkt geovctrs_rect
#' @export
vec_ptype2.wk_wkt.geovctrs_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkt()
}
