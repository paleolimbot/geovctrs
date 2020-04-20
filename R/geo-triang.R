
#' Triangles
#'
#' The [geo_triang()] type is useful as an efficient representation of
#' triangles stored using column vectors.
#' Note that if any of `x0`, `y0`, `x1`, `y1`, `x2` or `y2` are missing
#' the triangles is interpreted as an empty polygon, whereas
#' `geo_triang(NA, NA, NA, NA, srid = NA)`
#' is "missing" (see [geo_is_missing()] and [geo_is_empty()]). Infinite
#' values (`Inf` and `-Inf`) can be used.
#'
#' @param x0, y0, x1, y1, x2, y2 Border values, recycled to a common
#'   length using [vctrs::vec_recycle_common()].
#' @inheritParams geo_srid
#'
#' @return A [new_geovctrs_triang()]
#' @export
#'
#' @examples
#' x <- geo_triang(x0 = 0:5, y0 = 0:5, x1 = 2:7, y1 = 1:6,
#'                     x2 = 1:6, y2 = 2:7)
#' #geo_plot(x)
#'
geo_triang <- function(x0 = double(), y0 = double(),
                       x1 = double(), y1 = double(),
                       x2 = double(), y2 = double(), srid = 0) {
  result <- new_geovctrs_triang(
    vec_recycle_common(
      x0 = vec_cast(x0, double()),
      y0 = vec_cast(y0, double()),
      x1 = vec_cast(x1, double()),
      y1 = vec_cast(y1, double()),
      x2 = vec_cast(x2, double()),
      y2 = vec_cast(y2, double()),
      srid = as_geo_srid(srid)
    )
  )

  result
}

#' S3 details for geovctrs_triang
#'
#' @param x A (possibly) [geo_triang()]
#' @inheritParams new_geovctrs_xy
#'
#' @export
#'
new_geovctrs_triang <- function(x = list(x0 = double(), y0 = double(),
                                       x1 = double(), y1 = double(),
                                       x2 = double(), y2 = double(),
                                       srid = integer())) {
  vec_assert(x$x0, double())
  vec_assert(x$y0, double())
  vec_assert(x$x1, double())
  vec_assert(x$y1, double())
  vec_assert(x$x2, double())
  vec_assert(x$y2, double())
  vec_assert(x$srid, integer())
  new_rcrd(x, class = c("geovctrs_triang", "geovctr"))
}

#' @export
#' @rdname new_geovctrs_triang
is_geovctrs_triang <- function(x) {
  inherits(x, "geovctrs_triang")
}

#' @export
vec_ptype_abbr.geovctrs_triang <- function(x, ...) {
  "triang"
}

#' @export
format.geovctrs_triang <- function(x, ..., col = FALSE) {
  if (length(x) == 0) {
    return(character(0))
  }

  paste0(
    maybe_blue(
      "(",
      format(field(x, "x0"), trim = TRUE, ...),
      " ",
      format(field(x, "y0"), trim = TRUE, ...),
      col = col
    ),
    maybe_grey(if (use_utf8()) "\U2197" else  cli::symbol$ellipsis, col = col),
    maybe_blue(
      format(field(x, "x1"), trim = TRUE, ...),
      " ",
      format(field(x, "y1"), trim = TRUE, ...),
      ")",
      col = col
    ),
    maybe_grey(if (use_utf8()) "\U2197" else  cli::symbol$ellipsis, col = col),
    maybe_blue(
      format(field(x, "x2"), trim = TRUE, ...),
      " ",
      format(field(x, "y2"), trim = TRUE, ...),
      ")",
      col = col
    )
  )
}

#' @export
print.geovctrs_triang <- function(x, ...) {
  obj_print_header(x, ...)
  print_default_colour(format(x, ..., col = FALSE), format(x, ..., col = TRUE))
  obj_print_footer(x, ...)
  invisible(x)
}

# dyamically exported in zzz.R
pillar_shaft.geovctrs_triang <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format(x, col = TRUE))
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.geovctrs_triang <- function(x, ...) {
  as_tibble(vec_data(x), ...)
}

#' @export
#' @importFrom tibble as_tibble
as.data.frame.geovctrs_triang <- function(x, ...) {
  as.data.frame(as_tibble.geovctrs_triang(x, ...))
}

#' @export
#' @rdname new_geovctrs_triang
as_geo_triang <- function(x, ...) {
  UseMethod("as_geo_triang")
}

#' @export
#' @rdname new_geovctrs_triang
as_geo_triang.default <- function(x, ...) {
  vec_cast(x, geo_triang())
}

#' @method vec_cast geovctrs_triang
#' @export
#' @export vec_cast.geovctrs_triang
#' @rdname new_geovctrs_triang
vec_cast.geovctrs_triang <- function(x, to, ...) {
  UseMethod("vec_cast.geovctrs_triang")
}

#' @method vec_cast.geovctrs_triang default
#' @export
vec_cast.geovctrs_triang.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geovctrs_triang geovctrs_triang
#' @export
vec_cast.geovctrs_triang.geovctrs_triang <- function(x, to, ...) {
  x
}
#' @method vec_cast.geovctrs_wkt geovctrs_triang
#' @export
vec_cast.geovctrs_wkt.geovctrs_triang <- function(x, to, ...) {
  vec_cast(geo_xy(c(field(x, "x0"), field(x, "x1"), field(x, "x2")),
         c(field(x, "y0"), field(x, "y1"), field(x, "y2"))),
         to)

}
#' @method vec_cast.geovctrs_wkb geovctrs_triang
#' @export
vec_cast.geovctrs_wkb.geovctrs_triang <- function(x, to, ...) {
  vec_cast(geo_xy(c(field(x, "x0"), field(x, "x1"), field(x, "x2")),
                  c(field(x, "y0"), field(x, "y1"), field(x, "y2"))),
           to)

}

#' @method vec_cast.geovctrs_collection geovctrs_triang
#' @export
vec_cast.geovctrs_collection.geovctrs_triang <- function(x, to, ...) {
  vec_cast(geo_xy(c(field(x, "x0"), field(x, "x1"), field(x, "x2")),
                  c(field(x, "y0"), field(x, "y1"), field(x, "y2"))),
           to)

}
# ------------- prototypes ------------

#' @method vec_ptype2 geovctrs_triang
#' @export
#' @export vec_ptype2.geovctrs_triang
#' @rdname new_geovctrs_triang
vec_ptype2.geovctrs_triang <- function(x, y, ...) {
  UseMethod("vec_ptype2.geovctrs_triang", y)
}

#' @method vec_ptype2.geovctrs_triang default
#' @export
vec_ptype2.geovctrs_triang.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geovctrs_triang geovctrs_triang
#' @export
vec_ptype2.geovctrs_triang.geovctrs_triang <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_triang()
}

#' @method vec_ptype2.geovctrs_triang geovctrs_wkt
#' @export
vec_ptype2.geovctrs_triang.geovctrs_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkt()
}

#' @method vec_ptype2.geovctrs_triang geovctrs_wkb
#' @export
vec_ptype2.geovctrs_triang.geovctrs_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_wkb()
}

#' @method vec_ptype2.geovctrs_triang geovctrs_collection
#' @export
vec_ptype2.geovctrs_triang.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_collection()
}


