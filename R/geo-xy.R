
#' Create a coordinate vector
#'
#' @param x,y x and y coordinates
#'
#' @return A [new_geo_xy()]
#' @export
#'
#' @examples
#' geo_xy(0:5, 1:6)
#'
geo_xy <- function(x = double(), y = double()) {
  new_geo_xy(vec_recycle_common(x = vec_cast(x, double()), y = vec_cast(y, double())))
}

#' S3 details for geo_xy
#'
#' @param x A (possibly) [geo_xy()]
#' @param ... Unused
#' @param to,y Arguments to [vctrs::vec_cast()] and [vctrs::vec_ptype2()]
#'
#' @export
#'
new_geo_xy <- function(x = list(x = double(), y = double())) {
  vec_assert(x$x, double())
  vec_assert(x$y, double())
  new_rcrd(x, class = "geo_xy")
}

#' @export
#' @rdname new_geo_xy
is_geo_xy <- function(x) {
  inherits(x, "geo_xy")
}

#' @export
#' @rdname new_geo_xy
validate_geo_xy <- function(x) {
  abort("not implemented")
}

#' @export
vec_ptype_abbr.geo_xy <- function(x, ...) {
  "xy"
}

#' @export
format.geo_xy <- function(x, ...) {
  sprintf(
    "(%s %s)",
    format(field(x, "x"), trim = TRUE, ...),
    format(field(x, "y"), trim = TRUE, ...)
  )
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.geo_xy <- function(x, ...) {
  as_tibble(vec_data(x), ...)
}

#' @export
#' @importFrom tibble as_tibble
as.data.frame.geo_xy <- function(x, ...) {
  as.data.frame(as_tibble.geo_xy(x, ...))
}

#' @export
#' @rdname new_geo_xy
as_geo_xy.matrix <- function(x, ...) {
  names <- colnames(x)
  if (all(c("x", "y") %in% names)) {
    x_col <- match("x", names)
    y_col <- match("y", names)
  } else {
    x_col <- 1
    y_col <- 2
  }

  geo_xy(x = x[, x_col, drop = TRUE], y = x[, y_col, drop = TRUE])
}

#' @export
#' @rdname new_geo_xy
as.matrix.geo_xy <- function(x, ...) {
  as.matrix(as.data.frame(x))
}

#' @export
#' @rdname new_geo_xy
as_geo_xy <- function(x, ...) {
  UseMethod("as_geo_xy")
}

#' @export
#' @rdname new_geo_xy
as_geo_xy.default <- function(x, ...) {
  vec_cast(x, geo_xy())
}

#' @method vec_cast geo_xy
#' @export
#' @export vec_cast.geo_xy
#' @rdname new_geo_xy
vec_cast.geo_xy <- function(x, to, ...) {
  UseMethod("vec_cast.geo_xy")
}

#' @method vec_cast.geo_xy default
#' @export
vec_cast.geo_xy.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_xy geo_xy
#' @export
vec_cast.geo_xy.geo_xy <- function(x, to, ...) {
  x
}

#' @method vec_cast.geo_xy geo_wkt
#' @export
vec_cast.geo_xy.geo_wkt <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_xy geo_wkb
#' @export
vec_cast.geo_xy.geo_wkb <- function(x, to, ...) {
  cpp_convert(x, to)
}

#' @method vec_cast.geo_xy geo_collection
#' @export
vec_cast.geo_xy.geo_collection <- function(x, to, ...) {
  cpp_convert(x, to)
}

# ------------- prototypes ------------

#' @method vec_ptype2 geo_xy
#' @export
#' @export vec_ptype2.geo_xy
#' @rdname new_geo_xy
vec_ptype2.geo_xy <- function(x, y, ...) {
  UseMethod("vec_ptype2.geo_xy", y)
}

#' @method vec_ptype2.geo_xy default
#' @export
vec_ptype2.geo_xy.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.geo_xy geo_xy
#' @export
vec_ptype2.geo_xy.geo_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_xy()
}

#' @method vec_ptype2.geo_xy geo_wkt
#' @export
vec_ptype2.geo_xy.geo_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geo_xy geo_wkb
#' @export
vec_ptype2.geo_xy.geo_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geo_xy geo_collection
#' @export
vec_ptype2.geo_xy.geo_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}
