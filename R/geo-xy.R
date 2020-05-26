
#' Create a coordinate vector
#'
#' The [geo_xy()] type is useful as an efficient representation of points
#' stored using column vectors. Note that `geo_xy(NA, NA)` is considered
#' an "empty" point rather than a "missing" point (see [geo_is_missing()]
#' and [geo_is_empty()]).
#'
#' @param x,y,z x, y, and z coordinate vectors
#'
#' @return A [new_geovctrs_xy()]
#' @export
#'
#' @examples
#' geo_xy(0:5, 1:6)
#' geo_plot(geo_xy(0:5, 1:6))
#'
#' geo_xyz(0:5, 1:6, 3)
#'
geo_xy <- function(x = double(), y = double()) {
  new_geovctrs_xy(vec_recycle_common(x = vec_cast(x, double()), y = vec_cast(y, double())))
}

#' S3 details for geovctrs_xy
#'
#' @param x A (possibly) [geo_xy()]
#' @param ... Unused
#' @param to,y Arguments to [vctrs::vec_cast()] and [vctrs::vec_ptype2()]
#'
#' @export
#'
new_geovctrs_xy <- function(x = list(x = double(), y = double())) {
  vec_assert(x$x, double())
  vec_assert(x$y, double())
  new_rcrd(x, class = c("geovctrs_xy", "geovctr"))
}

#' @export
#' @rdname new_geovctrs_xy
is_geovctrs_xy <- function(x) {
  inherits(x, "geovctrs_xy")
}

#' @export
vec_ptype_abbr.geovctrs_xy <- function(x, ...) {
  "xy"
}

#' @export
format.geovctrs_xy <- function(x, ...) {
  sprintf(
    "(%s %s)",
    format(field(x, "x"), trim = TRUE, ...),
    format(field(x, "y"), trim = TRUE, ...)
  )
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.geovctrs_xy <- function(x, ...) {
  as_tibble(vec_data(x), ...)
}

#' @export
#' @importFrom tibble as_tibble
as.data.frame.geovctrs_xy <- function(x, ...) {
  as.data.frame(as_tibble.geovctrs_xy(x, ...))
}

#' @export
#' @rdname new_geovctrs_xy
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
#' @rdname new_geovctrs_xy
as.matrix.geovctrs_xy <- function(x, ...) {
  as.matrix(as.data.frame(x))
}

#' @export
#' @rdname new_geovctrs_xy
as_geo_xy <- function(x, ...) {
  UseMethod("as_geo_xy")
}

#' @export
#' @rdname new_geovctrs_xy
as_geo_xy.default <- function(x, ...) {
  vec_cast(x, geo_xy())
}

#' @method vec_cast geovctrs_xy
#' @export
#' @export vec_cast.geovctrs_xy
#' @rdname new_geovctrs_xy
vec_cast.geovctrs_xy <- function(x, to, ...) {
  UseMethod("vec_cast.geovctrs_xy")
}

#' @method vec_cast.geovctrs_xy default
#' @export
vec_cast.geovctrs_xy.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geovctrs_xy geovctrs_xy
#' @export
vec_cast.geovctrs_xy.geovctrs_xy <- function(x, to, ...) {
  x
}

#' @method vec_cast.geovctrs_xy geovctrs_xyz
#' @export
vec_cast.geovctrs_xy.geovctrs_xyz <- function(x, to, ...) {
  x_data <- vec_data(x)
  result <- new_geovctrs_xy(list(x = x_data$x, y = x_data$y))
  maybe_lossy_cast(result, x, to, lossy = !is.na(x_data$z), ...)
  result
}

#' @method vec_cast.geovctrs_xy wk_wkt
#' @export
vec_cast.geovctrs_xy.wk_wkt <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_xy wk_wkb
#' @export
vec_cast.geovctrs_xy.wk_wkb <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

#' @method vec_cast.geovctrs_xy geovctrs_collection
#' @export
vec_cast.geovctrs_xy.geovctrs_collection <- function(x, to, ...) {
  geovctrs_cpp_convert(x, to)
}

# ------------- prototypes ------------

#' @method vec_ptype2 geovctrs_xy
#' @export
#' @export vec_ptype2.geovctrs_xy
#' @rdname new_geovctrs_xy
vec_ptype2.geovctrs_xy <- function(x, y, ...) {
  UseMethod("vec_ptype2.geovctrs_xy", y)
}

#' @method vec_ptype2.geovctrs_xy default
#' @export
vec_ptype2.geovctrs_xy.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geovctrs_xy geovctrs_xy
#' @export
vec_ptype2.geovctrs_xy.geovctrs_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_xy()
}

#' @method vec_ptype2.geovctrs_xy geovctrs_xyz
#' @export
vec_ptype2.geovctrs_xy.geovctrs_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_xyz()
}

#' @method vec_ptype2.geovctrs_xy wk_wkt
#' @export
vec_ptype2.geovctrs_xy.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geovctrs_xy wk_wkb
#' @export
vec_ptype2.geovctrs_xy.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geovctrs_xy geovctrs_collection
#' @export
vec_ptype2.geovctrs_xy.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}
