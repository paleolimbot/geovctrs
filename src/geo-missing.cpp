
#include "geos-operator.h"
#include "geos-coords.h"
using namespace Rcpp;

class HasMissingOperator: public UnaryVectorOperator<LogicalVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    if (GEOSisEmpty_r(this->context, geometry)) {
      return false;
    } else {
      List coords = geometry_to_geo_coord(context, geometry);
      List xy = coords["xy"];
      NumericVector x = as<NumericVector>(xy["x"]);
      NumericVector y = as<NumericVector>(xy["y"]);
      bool naX = any(is_na(x));
      bool naY = any(is_na(y));
      return naX || naY;
    }
  }
};

// [[Rcpp::export]]
LogicalVector cpp_has_missing(SEXP x) {
  HasMissingOperator op;
  op.initProvider(x);
  return op.operate();
}

class IsFiniteOperator: public UnaryVectorOperator<LogicalVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    if (GEOSisEmpty_r(this->context, geometry)) {
      return true;
    } else {
      List coords = geometry_to_geo_coord(context, geometry);
      List xy = coords["xy"];
      NumericVector x = as<NumericVector>(xy["x"]);
      NumericVector y = as<NumericVector>(xy["y"]);
      bool finiteX = all(is_finite(x));
      bool finiteY = all(is_finite(y));
      return finiteX && finiteY;
    }
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_finite(SEXP x) {
  IsFiniteOperator op;
  op.initProvider(x);
  return op.operate();
}

class IsEmptyOperator: public UnaryVectorOperator<LogicalVector, bool> {
public:

  bool operateNext(GEOSGeometry* geometry)  {
    return GEOSisEmpty_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_empty(SEXP data) {
  IsEmptyOperator op;
  op.initProvider(data);
  return op.operate();
}
