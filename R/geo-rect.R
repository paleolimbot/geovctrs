
#' Rectangles
#'
#' The [geo_rect()] type is useful as an efficient representation of
#' rectangles stored using column vectors.
#' Note that if any of `xmin`, `ymin`, `xmax`, or `ymax` are missing
#' the rectangle is interpreted as an empty polygon, whereas
#' `geo_rect(NA, NA, NA, NA, srid = NA)`
#' is "missing" (see [geo_is_missing()] and [geo_is_empty()]). Infinite
#' values (`Inf` and `-Inf`) can be used.
#'
#' @param xmin,ymin,xmax,ymax Border values, recycled to a common
#'   length using [vctrs::vec_recycle_common()].
#' @inheritParams geo_srid
#'
#' @return A [new_geovctrs_rect()]
#' @export
#'
#' @examples
#' plot(geo_rect(xmin = 0:5, ymin = 0:5, xmax = 2:7, ymax = 2:7))
#'
geo_rect <- function(xmin = double(), ymin = double(), xmax = double(), ymax = double(), srid = 0) {
  result <- new_geovctrs_rect(
    vec_recycle_common(
      xmin = vec_cast(xmin, double()),
      ymin = vec_cast(ymin, double()),
      xmax = vec_cast(xmax, double()),
      ymax = vec_cast(ymax, double()),
      srid = as_geo_srid(srid)
    )
  )

  result
}

#' S3 details for geovctrs_rect
#'
#' @param x A (possibly) [geo_rect()]
#' @inheritParams new_geovctrs_xy
#'
#' @export
#'
new_geovctrs_rect <- function(x = list(xmin = double(), ymin = double(),
                                  xmax = double(), ymax = double(), srid = integer())) {
  vec_assert(x$xmin, double())
  vec_assert(x$ymin, double())
  vec_assert(x$xmax, double())
  vec_assert(x$ymax, double())
  vec_assert(x$srid, integer())
  new_rcrd(x, class = "geovctrs_rect")
}

#' @export
#' @rdname new_geovctrs_rect
is_geovctrs_rect <- function(x) {
  inherits(x, "geovctrs_rect")
}

#' @export
vec_ptype_abbr.geovctrs_rect <- function(x, ...) {
  "rect"
}

#' @export
format.geovctrs_rect <- function(x, ...) {
  if (length(x) == 0) {
    return(character(0))
  }

  paste0(
    "(",
    format(field(x, "xmin"), trim = TRUE, ...),
    " ",
    format(field(x, "ymin"), trim = TRUE, ...),
    "...",
    format(field(x, "xmax"), trim = TRUE, ...),
    " ",
    format(field(x, "ymax"), trim = TRUE, ...),
    ")"
  )
}

#' @export
as.character.geovctrs_rect <- function(x, ...) {
  format(x, ...)
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.geovctrs_rect <- function(x, ...) {
  as_tibble(vec_data(x), ...)
}

#' @export
#' @importFrom tibble as_tibble
as.data.frame.geovctrs_rect <- function(x, ...) {
  as.data.frame(as_tibble.geovctrs_rect(x, ...))
}

#' @export
#' @rdname new_geovctrs_rect
as_geo_rect <- function(x, ...) {
  UseMethod("as_geo_rect")
}

#' @export
#' @rdname new_geovctrs_rect
as_geo_rect.default <- function(x, ...) {
  vec_cast(x, geo_rect())
}

#' @method vec_cast geovctrs_rect
#' @export
#' @export vec_cast.geovctrs_rect
#' @rdname new_geovctrs_rect
vec_cast.geovctrs_rect <- function(x, to, ...) {
  UseMethod("vec_cast.geovctrs_rect")
}

#' @method vec_cast.geovctrs_rect default
#' @export
vec_cast.geovctrs_rect.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geovctrs_rect geovctrs_rect
#' @export
vec_cast.geovctrs_rect.geovctrs_rect <- function(x, to, ...) {
  x
}

# ------------- prototypes ------------

#' @method vec_ptype2 geovctrs_rect
#' @export
#' @export vec_ptype2.geovctrs_rect
#' @rdname new_geovctrs_rect
vec_ptype2.geovctrs_rect <- function(x, y, ...) {
  UseMethod("vec_ptype2.geovctrs_rect", y)
}

#' @method vec_ptype2.geovctrs_rect default
#' @export
vec_ptype2.geovctrs_rect.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geovctrs_rect geovctrs_rect
#' @export
vec_ptype2.geovctrs_rect.geovctrs_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_rect()
}

#' @method vec_ptype2.geovctrs_rect wk_wkt
#' @export
vec_ptype2.geovctrs_rect.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkt()
}

#' @method vec_ptype2.geovctrs_rect wk_wkb
#' @export
vec_ptype2.geovctrs_rect.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkb()
}

#' @method vec_ptype2.geovctrs_rect wk_wksxp
#' @export
vec_ptype2.geovctrs_rect.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wksxp()
}
