
# nocov start
.onLoad <- function(...) {
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

  # register generics for soft dependencies
  register_s3_method("pillar", "pillar_shaft", "geovctr")
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
