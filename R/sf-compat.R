
#' @rdname is_geovctr
#' @export
as_geovctr.sfc <- function(x, ...) {
  wkb_list <- unclass(sf::st_as_binary(x, ..., EWKB = TRUE))
  new_geovctrs_wkb(wkb_list)
}

#' @rdname is_geovctr
#' @export
as_geovctr.sf <- function(x, ...) {
  as_geovctr(x[[attr(x, "sf_column")]], ...)
}

#' @rdname is_geovctr
#' @export
restore_geovctr.sfc <- function(x, result, ...) {
  wkb <- as_geo_wkb(result)
  wkb[is.na(wkb)] <- as_geo_wkb("GEOMETRYCOLLECTION EMPTY")
  class(wkb) <- "WKB"
  sf::st_as_sfc(wkb, ..., EWKB = TRUE)
}

#' @rdname is_geovctr
#' @export
restore_geovctr.sf <- function(x, result, ...) {
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
