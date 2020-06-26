
#' Line segments
#'
#' The [geo_segment()] type is useful as an efficient representation of
#' line segments stored using column vectors.
#'
#' @param x0,y0,x1,y1 Values for the start and end coordinates.
#' @inheritParams geo_srid
#'
#' @return A [new_geovctrs_segment()]
#' @export
#'
#' @examples
#' geo_plot(geo_segment(0, 0, 10, -10:10))
#'
geo_segment <- function(x0 = double(), y0 = double(),
                        x1 = double(), y1 = double(), srid = 0) {
  new_geovctrs_segment(
    vec_recycle_common(
      x0 = vec_cast(x0, double()),
      y0 = vec_cast(y0, double()),
      x1 = vec_cast(x1, double()),
      y1 = vec_cast(y1, double()),
      srid = as_geo_srid(srid)
    )
  )
}

#' S3 details for geovctrs_segment
#'
#' @param x A (possibly) [geo_segment()]
#' @inheritParams new_geovctrs_xy
#'
#' @export
#'
new_geovctrs_segment <- function(x = list(x0 = double(), y0 = double(),
                                          x1 = double(), y1 = double(), srid = integer())) {
  vec_assert(x$x0, double())
  vec_assert(x$y0, double())
  vec_assert(x$x1, double())
  vec_assert(x$y1, double())
  new_rcrd(x, class = c("geovctrs_segment", "geovctr"))
}

#' @export
#' @rdname new_geovctrs_segment
is_geovctrs_segment <- function(x) {
  inherits(x, "geovctrs_segment")
}

#' @export
#' @rdname new_geovctrs_segment
validate_geovctrs_segment <- function(x) {
  abort("not implemented")
}

#' @export
vec_ptype_abbr.geovctrs_segment <- function(x, ...) {
  "segment"
}

#' @export
format.geovctrs_segment <- function(x, ..., col = FALSE) {
  if (length(x) == 0) {
    return(character(0))
  }

  paste0(
    "(",
    format(field(x, "x0"), trim = TRUE, ...),
    " ",
    format(field(x, "y0"), trim = TRUE, ...),
    "---",
    format(field(x, "x1"), trim = TRUE, ...),
    " ",
    format(field(x, "y1"), trim = TRUE, ...),
    ")"
  )
}

#' @export
as.character.geovctrs_segment <- function(x, ...) {
  format(x, ...)
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.geovctrs_segment <- function(x, ...) {
  as_tibble(vec_data(x), ...)
}

#' @export
#' @importFrom tibble as_tibble
as.data.frame.geovctrs_segment <- function(x, ...) {
  as.data.frame(as_tibble.geovctrs_segment(x, ...))
}

# -------- casting ----------

#' @export
#' @rdname new_geovctrs_segment
as_geo_segment <- function(x, ...) {
  UseMethod("as_geo_segment")
}

#' @export
#' @rdname new_geovctrs_segment
as_geo_segment.default <- function(x, ...) {
  vec_cast(x, geo_segment())
}

#' @method vec_cast geovctrs_segment
#' @export
#' @export vec_cast.geovctrs_segment
#' @rdname new_geovctrs_segment
vec_cast.geovctrs_segment <- function(x, to, ...) {
  UseMethod("vec_cast.geovctrs_segment")
}

#' @method vec_cast.geovctrs_segment default
#' @export
vec_cast.geovctrs_segment.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geovctrs_segment geovctrs_segment
#' @export
vec_cast.geovctrs_segment.geovctrs_segment <- function(x, to, ...) {
  x
}

#' @method vec_cast.geovctrs_segment wk_wkt
#' @export
vec_cast.geovctrs_segment.wk_wkt <- function(x, to, ...) {
  new_geovctrs_segment(cpp_translate_wkt_segment(x, includeSRID = NA))
}

#' @method vec_cast.geovctrs_segment wk_wkb
#' @export
vec_cast.geovctrs_segment.wk_wkb <- function(x, to, ...) {
  new_geovctrs_segment(cpp_translate_wkb_segment(x, includeSRID = NA))
}

#' @method vec_cast.geovctrs_segment wk_wksxp
#' @export
vec_cast.geovctrs_segment.wk_wksxp <- function(x, to, ...) {
  new_geovctrs_segment(cpp_translate_wksxp_segment(x, includeSRID = NA))
}

# ------------- prototypes ------------

#' @method vec_ptype2 geovctrs_segment
#' @export
#' @export vec_ptype2.geovctrs_segment
#' @rdname new_geovctrs_segment
vec_ptype2.geovctrs_segment <- function(x, y, ...) {
  UseMethod("vec_ptype2.geovctrs_segment", y)
}

#' @method vec_ptype2.geovctrs_segment default
#' @export
vec_ptype2.geovctrs_segment.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geovctrs_segment geovctrs_segment
#' @export
vec_ptype2.geovctrs_segment.geovctrs_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_segment()
}

#' @method vec_ptype2.geovctrs_segment wk_wkt
#' @export
vec_ptype2.geovctrs_segment.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkt()
}

#' @method vec_ptype2.geovctrs_segment wk_wkb
#' @export
vec_ptype2.geovctrs_segment.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkb()
}

#' @method vec_ptype2.geovctrs_segment wk_wksxp
#' @export
vec_ptype2.geovctrs_segment.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wksxp()
}
