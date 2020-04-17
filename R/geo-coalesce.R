
#' Find first non-empty feature
#'
#' Like [dplyr::coalesce()], [geo_coalesce()] returns the first non-missing
#' element (rowwise). It is useful for replacing empty and missing values
#' with a default.
#'
#' @param ... Vectors that will be recycled to a common length. These don't
#'   necessarily need to be geovctrs, but do need methods for [geo_is_empty()]
#'   and must be castable to the type of the first argument.
#'
#' @return A vector with the type of the first argument.
#' @export
#'
#' @examples
#' wkt <- geo_wkt(c("POINT (30 10)", "POINT EMPTY", NA))
#' geo_coalesce(wkt, geo_wkt("POINT (0 0)"))
#'
geo_coalesce <- function(...) {
  values <- vec_recycle_common(...)

  if (length(values) == 0) {
    abort("At least one value is required")
  }

  out <- values[[1]]
  for (x in values[-1]) {
    out_empty <- geo_is_empty(out)
    out[out_empty] <- x[out_empty]
  }

  out
}
