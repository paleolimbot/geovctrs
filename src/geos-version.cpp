
#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string cpp_version_impl() {
  return GEOSversion();
}
