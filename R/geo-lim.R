
#' Vectorized one-dimensional limits
#'
#' Vectorized version of [range()] to support [geo_x_range()],
#' [geo_y_range()], and [geo_z_range()]. The [geo_range()] function
#' works like [range()], but also works with [geo_lim()] vectors.
#'
#' @param lower,upper The lower and upper bounds of the one-dimensional rage.
#' @param x A vector of numbers or [geo_lim()]s.
#' @inheritParams geo_bbox
#'
#' @return A [new_geovctrs_lim()].
#' @export
#'
#' @examples
#' geo_lim(3, 4)
#'
#' # works like range() but returns a geo_lim()
#' geo_range(1:100)
#'
#' # you can also pass geo_lim() vectors to geo_range()
#' geo_range(c(geo_lim(81, Inf), geo_lim(-100, 12)))
#'
geo_lim <- function(lower = double(), upper = double()) {
  new_geovctrs_lim(
    vec_recycle_common(
      lower = vec_cast(lower, double()),
      upper = vec_cast(upper, double())
    )
  )
}

#' @rdname geo_lim
#' @export
geo_range <- function(x, na.rm = FALSE, finite = FALSE) {
  UseMethod("geo_range")
}

#' @rdname geo_lim
#' @export
geo_range.default <- function(x, na.rm = FALSE, finite = FALSE) {
  lim_range <- range(x, na.rm = na.rm, finite = finite)
  geo_lim(lim_range[1], lim_range[2])
}

#' @rdname geo_lim
#' @export
geo_range.geovctrs_lim <- function(x, na.rm = FALSE, finite = FALSE) {
  lim_range <- range(
    c(field(x, "lower"), field(x, "upper")),
    na.rm = na.rm,
    finite = finite
  )

  geo_lim(lim_range[1], lim_range[2])
}

#' S3 details for geovctrs_lim
#'
#' @param x A (possibly) [geo_lim()]
#' @inheritParams new_geovctrs_xy
#'
#' @export
#'
new_geovctrs_lim <- function(x = list(lower = double(), upper = double())) {
  vec_assert(x$lower, double())
  vec_assert(x$upper, double())
  new_rcrd(x, class = "geovctrs_lim")
}

#' @rdname new_geovctrs_lim
#' @export
is_geovctrs_lim <- function(x) {
  inherits(x, "geovctrs_lim")
}

#' @export
vec_ptype_abbr.geovctrs_lim <- function(x, ...) {
  "lim"
}

#' @export
format.geovctrs_lim <- function(x, ...) {
  sprintf(
    "[%s, %s]",
    format(field(x, "lower"), trim = TRUE, ...),
    format(field(x, "upper"), trim = TRUE, ...)
  )
}

#' @rdname new_geovctrs_lim
#' @export
as_geo_lim <- function(x, ...) {
  UseMethod("as_geo_lim")
}

#' @export
as_geo_lim.default <- function(x, ...) {
  vec_cast(x, geo_lim())
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.geovctrs_lim <- function(x, ...) {
  as_tibble(vec_data(x), ...)
}

#' @export
#' @importFrom tibble as_tibble
as.data.frame.geovctrs_lim <- function(x, ...) {
  as.data.frame(as_tibble.geovctrs_lim(x, ...))
}

#' @export
#' @rdname new_geovctrs_lim
as_geo_lim.matrix <- function(x, ...) {
  names <- colnames(x)
  if (all(c("lower", "upper") %in% names)) {
    lower_col <- match("lower", names)
    upper_col <- match("upper", names)
  } else {
    lower_col <- 1
    upper_col <- 2
  }

  geo_lim(lower = x[, lower_col, drop = TRUE], upper = x[, upper_col, drop = TRUE])
}

#' @export
#' @rdname new_geovctrs_lim
as.matrix.geovctrs_lim <- function(x, ...) {
  as.matrix(as.data.frame(x))
}
