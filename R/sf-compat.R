
# dynamically registered in zzz.R
st_as_sf.geovctr <- function(x, ...) {
  sf::st_as_sf(tibble(geometry = st_as_sfc.geovctr(x, ...)))
}

# dynamically registered in zzz.R
st_as_sfc.geovctr <- function(x, ...) {
  vec_cast(x, sf::st_sfc())
}

#' @rdname is_geovctr
#' @export
as_geovctr.sfc <- function(x, ...) {
  vec_cast(x, wkb())
}

#' @rdname is_geovctr
#' @export
as_geovctr.sf <- function(x, ...) {
  as_geovctr(x[[attr(x, "sf_column")]], ...)
}

#' @rdname is_geovctr
#' @export
restore_geovctr.sfc <- function(x, result, ...) {
  st_as_sfc.geovctr(result, ...)
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


# ---- we also need cast methods for sfc for full support ----
# always prefer type that looses the least information (e.g., not WKT because WKT drops CRS)

register_sf_compat <- function() {
  register_s3_method("sf", "st_as_sfc", "geovctr")
  register_s3_method("sf", "st_as_sf", "geovctr")

  register_s3_method("sf", "vec_cast.sfc", "wk_wkb")
  register_s3_method("sf", "vec_cast.sfc", "wk_wkt")
  register_s3_method("sf", "vec_cast.sfc", "geovctrs_collection")
  register_s3_method("sf", "vec_cast.sfc", "geovctrs_xy")
  register_s3_method("sf", "vec_cast.sfc", "geovctrs_segment")
  register_s3_method("sf", "vec_cast.sfc", "geovctrs_rect")

  register_s3_method("sf", "vec_ptype2.sfc", "wk_wkb")
  register_s3_method("sf", "vec_ptype2.sfc", "wk_wkt")
  register_s3_method("sf", "vec_ptype2.sfc", "geovctrs_collection")
  register_s3_method("sf", "vec_ptype2.sfc", "geovctrs_xy")
  register_s3_method("sf", "vec_ptype2.sfc", "geovctrs_segment")
  register_s3_method("sf", "vec_ptype2.sfc", "geovctrs_rect")
}

# ----- wkb ------

#' @method vec_ptype2.wk_wkb sfc
#' @export
vec_ptype2.wk_wkb.sfc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkb()
}

vec_ptype2.sfc.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wkb()
}

#' @method vec_cast.wk_wkb sfc
#' @export
vec_cast.wk_wkb.sfc <- function(x, to, ...) {
  wkb_list <- unclass(sf::st_as_binary(x, ..., EWKB = TRUE))
  new_wk_wkb(wkb_list)
}

vec_cast.sfc.wk_wkb <- function(x, to, ...) {
  wkb <- as_wkb(x)
  wkb[is.na(wkb)] <- as_wkb("GEOMETRYCOLLECTION EMPTY")
  class(wkb) <- "WKB"
  sf::st_as_sfc(wkb, ..., EWKB = TRUE)
}

# ----- wkt ------

#' @method vec_ptype2.wk_wkt sfc
#' @export
vec_ptype2.wk_wkt.sfc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

vec_ptype2.sfc.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

#' @method vec_cast.wk_wkt sfc
#' @export
vec_cast.wk_wkt.sfc <- function(x, to, ...) {
  # way faster than sf::st_as_text() for anything other than point
  vec_cast(vec_cast(x, wkb()), wkt())
}

vec_cast.sfc.wk_wkt <- function(x, to, ...) {
  # need to use this rather than sf::st_as_sfc() because missings
  # aren't handled by sf::st_as_sfc(), which uses OGR to parse
  vec_cast(vec_cast(x, wkb()), sf::st_sfc())
}

# ----- collection ------

#' @method vec_ptype2.geovctrs_collection sfc
#' @export
vec_ptype2.geovctrs_collection.sfc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

vec_ptype2.sfc.geovctrs_collection <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

#' @method vec_cast.geovctrs_collection sfc
#' @export
vec_cast.geovctrs_collection.sfc <- function(x, to, ...) {
  vec_cast(vec_cast(x, wkb()), geo_collection())
}

vec_cast.sfc.geovctrs_collection <- function(x, to, ...) {
  vec_cast(vec_cast(x, wkb()), sf::st_sfc())
}

# ----- xy ------

#' @method vec_ptype2.geovctrs_xy sfc
#' @export
vec_ptype2.geovctrs_xy.sfc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

vec_ptype2.sfc.geovctrs_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

#' @method vec_cast.geovctrs_xy sfc
#' @export
vec_cast.geovctrs_xy.sfc <- function(x, to, ...) {
  vec_cast(vec_cast(x, wkb()), geo_xy())
}

vec_cast.sfc.geovctrs_xy <- function(x, to, ...) {
  vec_cast(vec_cast(x, wkb()), sf::st_sfc())
}

# ----- segment ------

#' @method vec_ptype2.geovctrs_segment sfc
#' @export
vec_ptype2.geovctrs_segment.sfc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

vec_ptype2.sfc.geovctrs_segment <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

#' @method vec_cast.geovctrs_segment sfc
#' @export
vec_cast.geovctrs_segment.sfc <- function(x, to, ...) {
  vec_cast(vec_cast(x, wkb()), geo_segment())
}

vec_cast.sfc.geovctrs_segment <- function(x, to, ...) {
  vec_cast(vec_cast(x, wkb()), sf::st_sfc())
}

# ----- rect ------

#' @method vec_ptype2.geovctrs_rect sfc
#' @export
vec_ptype2.geovctrs_rect.sfc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

vec_ptype2.sfc.geovctrs_rect <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  sf::st_sfc()
}

vec_cast.sfc.geovctrs_rect <- function(x, to, ...) {
  vec_cast(vec_cast(x, wkb()), sf::st_sfc())
}
