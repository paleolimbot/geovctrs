
#include "geovctrs/geos-handler.hpp"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string cpp_geos_version_runtime() {
  return RcppGEOSHandler::runtimeVersion();
}

// [[Rcpp::export]]
std::string cpp_geos_version_build() {
  return RcppGEOSHandler::buildVersion();
}
