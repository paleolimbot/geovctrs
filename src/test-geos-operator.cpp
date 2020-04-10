
#include "geovctrs/geos-operator.hpp"
#include <Rcpp.h>
using namespace Rcpp;

class TestBufferOp: public UnaryGeometryOperator {
  GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    // width, segments per quandrant
    return GEOSBuffer_r(context, geometry, 2, 30);
  }
};

// [[Rcpp::export]]
SEXP cpp_test_buffer2(SEXP data, SEXP ptype) {
  TestBufferOp op;
  op.initProvider(data);
  op.initExporter(ptype);
  return op.operate();
}

// [[Rcpp::export]]
SEXP cpp_test_buffer2_bad_provider(SEXP data, SEXP ptype) {
  TestBufferOp op;
  op.initExporter(ptype);
  return op.operate();
}

// [[Rcpp::export]]
SEXP cpp_test_buffer2_bad_exporter(SEXP data, SEXP ptype) {
  TestBufferOp op;
  op.initProvider(data);
  return op.operate();
}
