
.onLoad <- function(libname, pkgname) {
  NA_wkt_ <<- geo_wkt()[NA_integer_]
  NA_wkb_ <<- geo_wkb()[NA_integer_]
  NA_collection_ <<- geo_collection()[NA_integer_]
  NA_xy_ <<- geo_xy()[NA_integer_]

  # something about the nature geo_segment()  doesn't work with
  # devtools::document(), but does work in all other namespace
  # loading (devtools::load_all(), library())
  # probably related to nested calling of geo_xy()
  NA_segment_ <<- try(geo_segment()[NA_integer_], silent = TRUE)

  NA_rect_ <<- geo_rect()[NA_integer_]
}
