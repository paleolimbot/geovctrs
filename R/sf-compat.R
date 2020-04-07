
sf_compat_as_binary <- function(x, ...) {
  pkg_fun("sf", "st_as_binary")(x, ...)
}

sf_compat_as_sfc <- function(x, ...) {
  pkg_fun("sf", "st_as_sfc")(x, ...)
}

pkg_fun <- function(pkg, name) {
  getNamespace(pkg)[[name]]
}
