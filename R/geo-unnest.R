
#' Unnest recursive geometry structures
#'
#' @inheritParams geo_bbox
#' @param keep_empty If `TRUE`, a GEOMETRYCOLLECTION EMPTY is left as-is
#'   rather than collapsing to length 0.
#' @param keep_multi If `TRUE`, MULTI* geometries are not expanded to sub-features.
#' @param max_depth The maximum recursive GEOMETRYCOLLECTION depth to unnest.
#'
#' @export
#'
#' @examples
#' geo_unnest("GEOMETRYCOLLECTION (POINT (1 2), POINT (3 4))")
#' geo_unnest("GEOMETRYCOLLECTION EMPTY")
#' geo_unnest("GEOMETRYCOLLECTION EMPTY", keep_empty = TRUE)
#'
#' geo_unnest(geo_example_wkt, keep_multi = TRUE)
#' geo_unnest(geo_example_wkt, keep_multi = FALSE)
#'
geo_unnest <- function(x, ..., keep_empty = FALSE, keep_multi = TRUE, max_depth = 1) {
  UseMethod("geo_unnest")
}

#' @rdname geo_unnest
#' @export
geo_unnest.default <- function(x, ..., keep_empty = FALSE, keep_multi = TRUE, max_depth = 1) {
  if (is_geovctr(x)) {
    geo_unnest(
      as_wksxp(x),
      keep_empty = keep_empty, keep_multi = keep_multi, max_depth = max_depth
    )
  } else {
    restore_geovctr(
      x,
      geo_unnest(
        as_geovctr(x),
        keep_empty = keep_empty,
        keep_multi = keep_multi,
        max_depth = max_depth
      )
    )
  }
}

#' @rdname geo_unnest
#' @export
geo_unnest.wk_wkt <- function(x, ..., keep_empty = FALSE, keep_multi = TRUE, max_depth = 1) {
  result <- wkutils::wkt_unnest(x, keep_empty, keep_multi, max_depth)
  attr(result, "lengths") <- NULL
  new_wk_wkt(result)
}

#' @rdname geo_unnest
#' @export
geo_unnest.wk_wkb <- function(x, ..., keep_empty = FALSE, keep_multi = TRUE, max_depth = 1) {
  result <- wkutils::wkb_unnest(x, keep_empty, keep_multi, max_depth)
  attr(result, "lengths") <- NULL
  new_wk_wkb(result)
}

#' @rdname geo_unnest
#' @export
geo_unnest.wk_wksxp <- function(x, ..., keep_empty = FALSE, keep_multi = TRUE, max_depth = 1) {
  result <- wkutils::wksxp_unnest(x, keep_empty, keep_multi, max_depth)
  attr(result, "lengths") <- NULL
  new_wk_wksxp(result)
}

# This is probably the most common use-case (data frames), hence the slight duplication
# of code to get this detail right. This is designed to also work on sf objects (or any
# data frame subclass).

#' @rdname geo_unnest
#' @export
geo_unnest.data.frame <- function(x, ..., keep_empty = FALSE, keep_multi = TRUE, max_depth = 1) {
  col <- as_geovctr(x)

  if (inherits(col, "wk_wkt")) {
    result <- wkutils::wkt_unnest(col, keep_empty, keep_multi, max_depth)
    lengths <- attr(result, "lengths")
    attr(result, "lengths") <- NULL
    result <- new_wk_wkt(result)
  } else if (inherits(col, "wk_wkb")) {
    result <- wkutils::wkb_unnest(col, keep_empty, keep_multi, max_depth)
    lengths <- attr(result, "lengths")
    attr(result, "lengths") <- NULL
    result <- new_wk_wkb(result)
  } else {
    result <- wkutils::wksxp_unnest(as_wksxp(col), keep_empty, keep_multi, max_depth)
    lengths <- attr(result, "lengths")
    attr(result, "lengths") <- NULL
    result <- new_wk_wksxp(result)
  }

  run_length_enc <- structure(
    list(lengths = lengths, values = seq_along(lengths)),
    class = "rle"
  )

  restore_geovctr(x[inverse.rle(run_length_enc), , drop = FALSE], result)
}
