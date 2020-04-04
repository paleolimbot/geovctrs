
.onLoad <- function(libname, pkgname) {
  NA_wkt_ <<- geo_wkt()[NA_integer_]
  NA_wkb_ <<- geo_wkb()[NA_integer_]
  NA_collection_ <<- geo_collection()[NA_integer_]
  NA_xy_ <<- geo_xy()[NA_integer_]
  NA_segment_ <<- geo_segment()[NA_integer_]
  NA_rect_ <<- geo_rect()[NA_integer_]
}
