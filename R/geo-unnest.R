
#' Unnest recursive geometry structures
#'
#' @inheritParams geo_bbox
#' @param keep_empty If `TRUE`, a GEOMETRYCOLLECTION EMPTY is left as-is
#'   rather than collapsing to length 0
#'
#' @export
#'
#' @examples
#' geo_unnest_collection("GEOMETRYCOLLECTION (POINT (1 2), POINT (3 4))")
#' geo_unnest_collection("GEOMETRYCOLLECTION EMPTY")
#' geo_unnest_collection("GEOMETRYCOLLECTION EMPTY", keep_empty = TRUE)
#'
geo_unnest_collection <- function(x, keep_empty = FALSE) {
  if (is_geovctr(x)) {
    sxp <- unclass(as_wksxp(x))
  } else {
    sxp <- unclass(as_wksxp(as_geovctr(x)))
  }

  if (length(sxp) != 1) {
    abort("`x` must be a geometry collection of length 1")
  }

  if (is.null(sxp[[1]])) {
    return(wksxp(list(NULL)))
  }

  if (!inherits(sxp[[1]], "wk_geometrycollection")) {
    abort("`x` must be a geometry collection of length 1")
  }

  if (keep_empty && length(sxp[[1]]) == 0) {
    restore_geovctr(x, new_wk_wksxp(sxp))
  } else {
    out <- lapply(sxp[[1]], function(item) {
      attr(item, "srid") <- attr(sxp[[1]], "srid")
      item
    })

    restore_geovctr(x, wksxp(out))
  }
}

#' @rdname geo_unnest_collection
#' @export
geo_unnest_all <- function(x, keep_empty = FALSE) {
  UseMethod("geo_unnest_all")
}

#' @rdname geo_unnest_collection
#' @export
geo_unnest_all.default <- function(x, keep_empty = FALSE) {
  if (is_geovctr(x)) {
    geo_unnest_all(as_wksxp(x), keep_empty)
  } else {
    restore_geovctr(x, geo_unnest_all(as_geovctr(x), keep_empty))
  }
}

#' @rdname geo_unnest_collection
#' @export
geo_unnest_all.wk_wkt <- function(x, keep_empty = FALSE) {
  new_wk_wkt(cpp_wkt_unnest_all(x, keep_empty))
}

#' @rdname geo_unnest_collection
#' @export
geo_unnest_all.wk_wkb <- function(x, keep_empty = FALSE) {
  new_wk_wkb(cpp_wkb_unnest_all(x, keep_empty, endian = wk::wk_platform_endian()))
}

#' @rdname geo_unnest_collection
#' @export
geo_unnest_all.wk_wksxp <- function(x, keep_empty = FALSE) {
  new_wk_wksxp(cpp_wksxp_unnest_all(x, keep_empty))
}
