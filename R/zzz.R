
# nocov start
.onLoad <- function(...) {
  NA_wkt_ <<- new_geovctrs_wkt()[NA_integer_]
  NA_wkb_ <<- new_geovctrs_wkb()[NA_integer_]
  NA_collection_ <<- geo_collection()[NA_integer_]
  NA_xy_ <<- geo_xy()[NA_integer_]

  # something about the nature of geo_segment() here specifically doesn't work
  # related to nested calling of geo_xy()
  NA_segment_ <<- new_geovctrs_segment(
    list(
      start = new_geovctrs_xy(list(x = NA_real_, y = NA_real_)),
      end = new_geovctrs_xy(list(x = NA_real_, y = NA_real_)),
      srid = NA_integer_
    )
  )

  NA_rect_ <<- geo_rect()[NA_integer_]

  # register generics for soft dependencies
  register_s3_method("pillar", "pillar_shaft", "geovctr")
  register_s3_method("pillar", "pillar_shaft", "geovctrs_rect")
  register_s3_method("pillar", "pillar_shaft", "geovctrs_segment")

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
