
#include "geos-operator.h"
#include "geos-coords.h"
using namespace Rcpp;

bool coords_have_missing(List item) {
  if (Rf_inherits(item, "geovctrs_collection")) {
    List features = item["feature"];
    bool anyNA = false;
    for (size_t i=0; i<features.size(); i++) {
      anyNA = anyNA || coords_have_missing(features[i]);
    }
    return anyNA;
  } else {
    List xy = item["xy"];
    NumericVector x = as<NumericVector>(xy["x"]);
    NumericVector y = as<NumericVector>(xy["y"]);
    bool naX = any(is_na(x));
    bool naY = any(is_na(y));
    return naX || naY;
  }
}

class HasMissingOperator: public UnaryVectorOperator<LogicalVector, int> {
  int operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (GEOSisEmpty_r(context, geometry)) {
      return false;
    } else {
      List coords = geometry_to_geo_coord(context, geometry);
      return coords_have_missing(coords);
    }
  }
};

// [[Rcpp::export]]
LogicalVector cpp_has_missing(SEXP x) {
  HasMissingOperator op;
  op.initProvider(x);
  return op.operate();
}

bool coords_is_finite(List item) {
  if (Rf_inherits(item, "geovctrs_collection")) {
    List features = item["feature"];
    bool isFinite = true;
    for (size_t i=0; i<features.size(); i++) {
      isFinite = isFinite && coords_is_finite(features[i]);
    }
    return isFinite;
  } else {
    List xy = item["xy"];
    NumericVector x = as<NumericVector>(xy["x"]);
    NumericVector y = as<NumericVector>(xy["y"]);
    bool finiteX = all(is_finite(x));
    bool finiteY = all(is_finite(y));
    return finiteX && finiteY;
  }
}

class IsFiniteOperator: public UnaryVectorOperator<LogicalVector, int> {
  int operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (GEOSisEmpty_r(context, geometry)) {
      return true;
    } else {
      List coords = geometry_to_geo_coord(context, geometry);
      return coords_is_finite(coords);
    }
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_finite(SEXP x) {
  IsFiniteOperator op;
  op.initProvider(x);
  return op.operate();
}

class IsEmptyOperator: public UnaryVectorOperator<LogicalVector, int> {
public:

  int operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i)  {
    return GEOSisEmpty_r(context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_empty(SEXP data) {
  IsEmptyOperator op;
  op.initProvider(data);
  return op.operate();
}
