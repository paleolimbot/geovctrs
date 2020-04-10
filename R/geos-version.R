
#' GEOS version information
#'
#' @export
#'
#' @examples
#' geos_version_runtime()
#' geos_version_build()
#'
geos_version_runtime <- function() {
  package_version(strsplit(geovctrs_cpp_geos_version_runtime(), "[- ]")[[1]][1])
}

#' @rdname geos_version_runtime
#' @export
geos_version_build <- function() {
  package_version(strsplit(geovctrs_cpp_geos_version_build(), "[- ]")[[1]][1])
}
