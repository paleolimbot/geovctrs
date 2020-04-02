
#include "geos-operator.h"
using namespace Rcpp;

class UnaryPredicateOperator: public UnaryVectorOperator<LogicalVector, bool> {
public:

  bool operateNext(GEOSGeometry* geometry)  {
    char result = this->operateNextGEOS(geometry);
    if (result == 2) {
      stop("Exception on binary predicate");
    } else if (result == 1) {
      return true;
    } else if (result == 0) {
      return  false;
    } else {
      stop("Unknown output from binary predicate");
    }
  }

  virtual char operateNextGEOS(GEOSGeometry* geometry) = 0;
};

class IsEmptyOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisEmpty_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_empty(SEXP data) {
  IsEmptyOperator op;
  op.initProvider(data);
  return op.operate();
}

class HasZOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSHasZ_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_has_z(SEXP data) {
  HasZOperator op;
  op.initProvider(data);
  return op.operate();
}

class GeomTypeIdOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGeomTypeId_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_geom_type_id(SEXP x) {
  GeomTypeIdOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetSRIDOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    int srid = GEOSGetSRID_r(this->context, geometry);
    if (srid == 0) {
      return NA_INTEGER;
    } else {
      return srid;
    }
  }
};

// [[Rcpp::export]]
IntegerVector cpp_get_srid(SEXP x) {
  GetSRIDOperator op;
  op.initProvider(x);
  return op.operate();
}

class SetSRIDOperator: public UnaryGeometryOperator {
public:
  IntegerVector srid;

  SetSRIDOperator(IntegerVector srid) {
    this->srid = srid;
  }

  size_t getMaxParameterLength() {
    return srid.size();
  }

  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    int srid = this->srid[this->counter];
    if (IntegerVector::is_na(srid)) {
      GEOSSetSRID_r(this->context, geometry, 0);
    } else {
      GEOSSetSRID_r(this->context, geometry, srid);
    }

    return geometry;
  }
};

// [[Rcpp::export]]
SEXP cpp_set_srid(SEXP x, IntegerVector srid) {
  SetSRIDOperator op(srid);
  // returning the same type as input
  op.initProvider(x, x);
  return op.operate();
}

class GetNumGeometriesOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGetNumGeometries_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_geometries(SEXP x) {
  GetNumGeometriesOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetNumCoordinatesOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGetNumCoordinates_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_coordinates(SEXP x) {
  GetNumCoordinatesOperator op;
  op.initProvider(x);
  return op.operate();
}
