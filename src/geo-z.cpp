
#include "geovctrs/geos/operator.hpp"
using namespace Rcpp;

class SetZOperator: public GeovctrsGEOSRecursiveGeometryOperator {
public:
  NumericVector z;

  SetZOperator(NumericVector z) {
    this->z = z;
  }

  R_xlen_t getMaxParameterLength() {
    return z.size();
  }

  GEOSCoordSequence* nextCoordinateSequence(GEOSContextHandle_t context,
                                            const GEOSGeometry* geometry,
                                            const GEOSCoordSequence* seq) {
    // need the geometry here because while GEOS doesn't do M
    // it may eventually, and this would be with the geom, not the
    // coord seq
    unsigned int size;
    GEOSCoordSeq_getSize_r(context, seq, &size);

    double xi, yi;
    GEOSCoordSequence* newSeq = GEOSCoordSeq_create_r(context, size, 3);
    for (unsigned int i=0; i < size; i++) {
      GEOSCoordSeq_getX_r(context, seq, i, &xi);
      GEOSCoordSeq_getY_r(context, seq, i, &yi);

      GEOSCoordSeq_setX_r(context, newSeq, i, xi);
      GEOSCoordSeq_setY_r(context, newSeq, i, yi);
      GEOSCoordSeq_setZ_r(context, newSeq, i, this->z[this->featureId]);
    }

    return newSeq;
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_set_z(SEXP x, NumericVector z) {
  SetZOperator op(z);
  // returning the same type as input
  op.initProvider(x);
  op.initExporter(x);
  return op.operate();
}

class DropZOperator: public GeovctrsGEOSRecursiveGeometryOperator {
public:

  GEOSCoordSequence* nextCoordinateSequence(GEOSContextHandle_t context,
                                            const GEOSGeometry* geometry,
                                            const GEOSCoordSequence* seq) {

    unsigned int size;
    GEOSCoordSeq_getSize_r(context, seq, &size);

    double xi, yi;
    GEOSCoordSequence* newSeq = GEOSCoordSeq_create_r(context, size, 2);
    for (unsigned int i=0; i < size; i++) {
      GEOSCoordSeq_getX_r(context, seq, i, &xi);
      GEOSCoordSeq_getY_r(context, seq, i, &yi);

      GEOSCoordSeq_setX_r(context, newSeq, i, xi);
      GEOSCoordSeq_setY_r(context, newSeq, i, yi);
    }

    return newSeq;
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_drop_z(SEXP x, SEXP to) {
  DropZOperator op;
  // returning the same type as input
  op.initProvider(x);
  op.initExporter(to);
  return op.operate();
}
