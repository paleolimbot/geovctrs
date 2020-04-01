
#' GEOS information
#'
#' @noRd
#'
#' @examples
#' geos_version()
#' geos_capi_version()
#'
geos_version <- function() {
  package_version(strsplit(cpp_version_impl(), "[- ]")[[1]][1])
}

geos_capi_version <- function() {
  package_version(strsplit(cpp_version_impl(), "[- ]")[[1]][3])
}
