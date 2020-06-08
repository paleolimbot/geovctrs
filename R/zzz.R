
# nocov start
.onLoad <- function(...) {
  NA_wkt_ <<- wk::new_wk_wkt(character())[NA_integer_]
  NA_wkb_ <<- new_wk_wkb()[NA_integer_]
  NA_collection_ <<- geo_collection()[NA_integer_]
  NA_xy_ <<- geo_xy()[NA_integer_]
  NA_segment_ <<- geo_segment()[NA_integer_]
  NA_rect_ <<- geo_rect()[NA_integer_]

  # register generics for soft dependencies
  vctrs::s3_register("pillar::pillar_shaft", "geovctr")

  # keep sf registrations together in sf-compat.R
  register_sf_compat()
}

# nocov end
