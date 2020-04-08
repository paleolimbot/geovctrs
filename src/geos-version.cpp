
#include "geos-base.h"
using namespace Rcpp;

// [[Rcpp::export]]
std::string cpp_version_impl() {
  return GEOSversion();
}
