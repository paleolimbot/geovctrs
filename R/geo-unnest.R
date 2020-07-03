
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
  new_wk_wkt(cpp_wkt_unnest(x,keep_empty, keep_multi, max_depth))
}

#' @rdname geo_unnest
#' @export
geo_unnest.wk_wkb <- function(x, ..., keep_empty = FALSE, keep_multi = TRUE, max_depth = 1) {
  new_wk_wkb(cpp_wkb_unnest(x, keep_empty, keep_multi, max_depth, endian = wk::wk_platform_endian()))
}

#' @rdname geo_unnest
#' @export
geo_unnest.wk_wksxp <- function(x, ..., keep_empty = FALSE, keep_multi = TRUE, max_depth = 1) {
  new_wk_wksxp(cpp_wksxp_unnest(x, keep_empty, keep_multi, max_depth))
}
