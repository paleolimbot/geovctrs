
#include "geovctrs/geos-operator.hpp"
using namespace Rcpp;

class IdentityOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    return geometry;
  }
};

// [[Rcpp::export]]
SEXP cpp_convert(SEXP data, SEXP ptype) {
  IdentityOperator op;
  op.initProvider(data);
  op.initExporter(ptype);
  return op.operate();
}
