
#include "geovctrs/geos-handler.hpp"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string geovctrs_cpp_geos_version_runtime() {
  return GeovctrsGEOSHandler::runtimeVersion();
}

// [[Rcpp::export]]
std::string geovctrs_cpp_geos_version_build() {
  return GeovctrsGEOSHandler::buildVersion();
}
