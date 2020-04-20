
#include "geovctrs/geos/operator.hpp"
using namespace Rcpp;

class SetSRIDOperator: public GeovctrsGEOSGeometryOperator {
public:
  IntegerVector srid;

  SetSRIDOperator(IntegerVector srid) {
    this->srid = srid;
  }

  R_xlen_t getMaxParameterLength() {
    return srid.size();
  }

  GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    if (geometry != NULL) {
      GEOSSetSRID_r(context, geometry, this->srid[i]);
    }

    return geometry;
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_set_srid(SEXP x, IntegerVector srid) {
  SetSRIDOperator op(srid);
  // returning the same type as input
  op.initProvider(x);
  op.initExporter(x);
  return op.operate();
}
