
#include "geovctrs/geos/operator.hpp"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

class IdentityOperator: public GeovctrsGEOSGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    return geometry;
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_convert(SEXP data, SEXP ptype) {
  IdentityOperator op;
  op.initProvider(data);
  op.initExporter(ptype);
  return op.operate();
}
