
#' @rdname geo_xy
#' @export
geo_xyz <- function(x = double(), y = double(), z = double()) {
  new_geovctrs_xyz(
    vec_recycle_common(
      x = vec_cast(x, double()),
      y = vec_cast(y, double()),
      z = vec_cast(z, double())
    )
  )
}

#' @rdname new_geovctrs_xy
#' @export
new_geovctrs_xyz <- function(x = list(x = double(), y = double(), z = double())) {
  vec_assert(x$x, double())
  vec_assert(x$y, double())
  vec_assert(x$z, double())
  new_rcrd(x, class = c("geovctrs_xyz", "geovctrs_xy"))
}

#' @export
#' @rdname new_geovctrs_xy
is_geovctrs_xyz <- function(x) {
  inherits(x, "geovctrs_xyz")
}

#' @export
vec_ptype_abbr.geovctrs_xyz <- function(x, ...) {
  "xyz"
}

#' @export
format.geovctrs_xyz <- function(x, ...) {
  sprintf(
    "(%s %s %s)",
    format(field(x, "x"), trim = TRUE, ...),
    format(field(x, "y"), trim = TRUE, ...),
    format(field(x, "z"), trim = TRUE, ...)
  )
}

#' @export
as.character.geovctrs_xyz <- function(x, ...) {
  format(x, ...)
}

#' @export
#' @rdname new_geovctrs_xy
as_geo_xyz.matrix <- function(x, ...) {
  names <- colnames(x)
  if (all(c("x", "y", "z") %in% names)) {
    x_col <- match("x", names)
    y_col <- match("y", names)
    z_col <- match("z", names)
  } else {
    x_col <- 1
    y_col <- 2
    z_col <- 3
  }

  geo_xyz(x = x[, x_col, drop = TRUE], y = x[, y_col, drop = TRUE], z = x[, z_col, drop = TRUE])
}

#' @export
#' @rdname new_geovctrs_xy
as_geo_xyz <- function(x, ...) {
  UseMethod("as_geo_xyz")
}

#' @export
#' @rdname new_geovctrs_xy
as_geo_xyz.default <- function(x, ...) {
  vec_cast(x, geo_xyz())
}

#' @method vec_cast geovctrs_xyz
#' @export
#' @export vec_cast.geovctrs_xyz
#' @rdname new_geovctrs_xy
vec_cast.geovctrs_xyz <- function(x, to, ...) {
  UseMethod("vec_cast.geovctrs_xyz")
}

#' @method vec_cast.geovctrs_xyz default
#' @export
vec_cast.geovctrs_xyz.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geovctrs_xyz geovctrs_xyz
#' @export
vec_cast.geovctrs_xyz.geovctrs_xyz <- function(x, to, ...) {
  x
}

#' @method vec_cast.geovctrs_xyz geovctrs_xy
#' @export
vec_cast.geovctrs_xyz.geovctrs_xy <- function(x, to, ...) {
  x <- vec_data(x)
  x$z <- rep_len(NA_real_, length(x$x))
  new_geovctrs_xyz(x)
}

#' @method vec_cast.geovctrs_xyz wk_wkt
#' @export
vec_cast.geovctrs_xyz.wk_wkt <- function(x, to, ...) {
  new_geovctrs_xyz(cpp_translate_wkt_xyz(x))
}

#' @method vec_cast.geovctrs_xyz wk_wkb
#' @export
vec_cast.geovctrs_xyz.wk_wkb <- function(x, to, ...) {
  new_geovctrs_xyz(cpp_translate_wkb_xyz(x))
}

#' @method vec_cast.geovctrs_xyz wk_wksxp
#' @export
vec_cast.geovctrs_xyz.wk_wksxp <- function(x, to, ...) {
  new_geovctrs_xyz(cpp_translate_wksxp_xyz(x))
}

# ------------- prototypes ------------

#' @method vec_ptype2 geovctrs_xyz
#' @export
#' @export vec_ptype2.geovctrs_xyz
#' @rdname new_geovctrs_xy
vec_ptype2.geovctrs_xyz <- function(x, y, ...) {
  UseMethod("vec_ptype2.geovctrs_xyz", y)
}

#' @method vec_ptype2.geovctrs_xyz default
#' @export
vec_ptype2.geovctrs_xyz.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geovctrs_xyz geovctrs_xyz
#' @export
vec_ptype2.geovctrs_xyz.geovctrs_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_xyz()
}

#' @method vec_ptype2.geovctrs_xyz geovctrs_xy
#' @export
vec_ptype2.geovctrs_xyz.geovctrs_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_xyz()
}

#' @method vec_ptype2.geovctrs_xyz wk_wkt
#' @export
vec_ptype2.geovctrs_xyz.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkt()
}

#' @method vec_ptype2.geovctrs_xyz wk_wkb
#' @export
vec_ptype2.geovctrs_xyz.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkb()
}

#' @method vec_ptype2.geovctrs_xyz wk_wksxp
#' @export
vec_ptype2.geovctrs_xyz.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wksxp()
}
