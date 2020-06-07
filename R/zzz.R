
# nocov start
.onLoad <- function(...) {
  NA_wkt_ <<- wk::new_wk_wkt(character())[NA_integer_]
  NA_wkb_ <<- new_wk_wkb()[NA_integer_]
  NA_collection_ <<- geo_collection()[NA_integer_]
  NA_xy_ <<- geo_xy()[NA_integer_]
  NA_segment_ <<- geo_segment()[NA_integer_]
  NA_rect_ <<- geo_rect()[NA_integer_]

  # register generics for soft dependencies
  register_s3_method("pillar", "pillar_shaft", "geovctr")

  # keep sf registrations together in sf-compat.R
  register_sf_compat()
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
