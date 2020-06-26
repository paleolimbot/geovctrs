
#' @rdname is_geovctr
#' @export
as_geovctr.sfc <- function(x, ...) {
  new_wk_wkb(unclass(sf::st_as_binary(x, EWKB = TRUE)))
}

#' @rdname is_geovctr
#' @export
as_geovctr.sf <- function(x, ...) {
  as_geovctr(x[[attr(x, "sf_column")]], ...)
}

#' @rdname is_geovctr
#' @export
restore_geovctr.sfc <- function(x, result, ...) {
  wkb <- unclass(as_wkb(result))
  wkb[vapply(wkb, is.null, logical(1))] <- as_wkb("GEOMETRYCOLLECTION EMPTY")
  sf::st_as_sfc(structure(wkb, class = "WKB"), EWKB = TRUE)
}

#' @rdname is_geovctr
#' @export
restore_geovctr.sf <- function(x, result, ...) {
  x <- vec_recycle(x, vec_size(result))
  x[[attr(x, "sf_column")]] <- restore_geovctr(x[[attr(x, "sf_column")]], result, ...)
  x
}

#' @rdname geo_plot
#' @export
geo_plot_add.sf <- function(x, ...) {
  geometry_col <- attr(x, "sf_column")
  as_df <- tibble::as_tibble(unclass(x))
  as_df[[geometry_col]] <- as_geovctr(x)
  geo_plot_add(as_df, ...)
}
