
#' Rectangles
#'
#' @param xmin,ymin,xmax,ymax Border values, recycled to a common
#'   length using [vctrs::vec_recycle_common()].
#' @inheritParams geo_srid
#'
#' @return A [new_geo_rect()]
#' @export
#'
#' @examples
#' geo_rect(xmin = 0:5, ymin = 0:5, xmax = 2:7, ymax = 2:7)
#'
geo_rect <- function(xmin = double(), ymin = double(), xmax = double(), ymax = double(), srid = 0) {
  result <- new_geo_rect(
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

#' S3 details for geo_rect
#'
#' @param x A (possibly) [geo_rect()]
#' @inheritParams new_geo_xy
#'
#' @export
#'
new_geo_rect <- function(x = list(xmin = double(), ymin = double(),
                                  xmax = double(), ymax = double(), srid = integer())) {
  vec_assert(x$xmin, double())
  vec_assert(x$ymin, double())
  vec_assert(x$xmax, double())
  vec_assert(x$ymax, double())
  vec_assert(x$srid, integer())
  new_rcrd(x, class = c("geo_rect", "geovctr"))
}

#' @export
#' @rdname new_geo_rect
is_geo_rect <- function(x) {
  inherits(x, "geo_rect")
}

#' @export
#' @rdname new_geo_rect
validate_geo_rect <- function(x) {
  abort("not implemented")
}

#' @export
vec_ptype_abbr.geo_rect <- function(x, ...) {
  "rect"
}

#' @export
format.geo_rect <- function(x, ...) {
  sprintf(
    "(%s %s)x(%s %s)",
    format(field(x, "xmin"), trim = TRUE, ...),
    format(field(x, "ymin"), trim = TRUE, ...),
    format(field(x, "xmax"), trim = TRUE, ...),
    format(field(x, "ymax"), trim = TRUE, ...)
  )
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.geo_rect <- function(x, ...) {
  as_tibble(vec_data(x), ...)
}

#' @export
#' @importFrom tibble as_tibble
as.data.frame.geo_rect <- function(x, ...) {
  as.data.frame(as_tibble.geo_rect(x, ...))
}

#' @export
#' @rdname new_geo_rect
as_geo_rect <- function(x, ...) {
  UseMethod("as_geo_rect")
}

#' @export
#' @rdname new_geo_rect
as_geo_rect.default <- function(x, ...) {
  vec_cast(x, geo_rect())
}

#' @method vec_cast geo_rect
#' @export
#' @export vec_cast.geo_rect
#' @rdname new_geo_rect
vec_cast.geo_rect <- function(x, to, ...) {
  UseMethod("vec_cast.geo_rect")
}

#' @method vec_cast.geo_rect default
#' @export
vec_cast.geo_rect.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_rect geo_rect
#' @export
vec_cast.geo_rect.geo_rect <- function(x, to, ...) {
  x
}

# ------------- prototypes ------------

#' @method vec_ptype2 geo_rect
#' @export
#' @export vec_ptype2.geo_rect
#' @rdname new_geo_rect
vec_ptype2.geo_rect <- function(x, y, ...) {
  UseMethod("vec_ptype2.geo_rect", y)
}

#' @method vec_ptype2.geo_rect default
#' @export
vec_ptype2.geo_rect.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.geo_rect geo_rect
#' @export
vec_ptype2.geo_rect.geo_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_rect()
}

#' @method vec_ptype2.geo_rect geo_wkt
#' @export
vec_ptype2.geo_rect.geo_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geo_rect geo_wkb
#' @export
vec_ptype2.geo_rect.geo_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geo_rect geo_collection
#' @export
vec_ptype2.geo_rect.geo_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}
