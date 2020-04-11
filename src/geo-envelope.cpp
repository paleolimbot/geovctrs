
#include "geovctrs/operator.hpp"
#include "geovctrs/factory.hpp"
#include "geovctrs/feature-factory.hpp"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

double min_reg(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i);
  bool x2NA = NumericVector::is_na(x2i);
  if (x1NA || x2NA) {
    return NA_REAL;
  } else {
    return std::min(x1i, x2i);
  }
}

double max_reg(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i);
  bool x2NA = NumericVector::is_na(x2i);
  if (x1NA || x2NA) {
    return NA_REAL;
  } else {
    return std::max(x1i, x2i);
  }
}

double min_na_rm(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i);
  bool x2NA = NumericVector::is_na(x2i);
  if (x1NA && x2NA) {
    return R_PosInf;
  } else if (x1NA || x2NA) {
    return NA_REAL;
  } else {
    return std::min(x1i, x2i);
  }
}

double max_na_rm(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i);
  bool x2NA = NumericVector::is_na(x2i);
  if (x1NA && x2NA) {
    return R_NegInf;
  } else if (x1NA || x2NA) {
    return NA_REAL;
  } else {
    return std::max(x1i, x2i);
  }
}

// [[Rcpp::export]]
NumericVector pmin2(NumericVector x1, NumericVector x2)  {
  NumericVector out (x1.size());

  for (size_t i=0; i<out.size(); i++) {
    out[i] = min_na_rm(x1[i], x2[i]);
  }

  return out;
}



// [[Rcpp::export]]
NumericVector pmax2(NumericVector x1, NumericVector x2)  {
  NumericVector out (x1.size());

  for (size_t i=0; i<out.size(); i++) {
    out[i] = max_na_rm(x1[i], x2[i]);
  }

  return out;
}

NumericVector bounds_from_coords(List item, bool naRm) {
  if (Rf_inherits(item, "geovctrs_collection")) {
    List features = item["feature"];
    NumericVector bounds = NumericVector::create(R_PosInf, R_PosInf, R_NegInf, R_NegInf);
    if (naRm) {
      for (size_t i=0; i<features.size(); i++) {
        NumericVector itemBounds = bounds_from_coords(features[i], naRm);
        bounds[0] = min_na_rm(bounds[0], itemBounds[0]);
        bounds[1] = min_na_rm(bounds[1], itemBounds[1]);
        bounds[2] = max_na_rm(bounds[2], itemBounds[2]);
        bounds[3] = max_na_rm(bounds[3], itemBounds[3]);
      }
    } else {
      for (size_t i=0; i<features.size(); i++) {
        NumericVector itemBounds = bounds_from_coords(features[i], naRm);
        bounds[0] = min_reg(bounds[0], itemBounds[0]);
        bounds[1] = min_reg(bounds[1], itemBounds[1]);
        bounds[2] = max_reg(bounds[2], itemBounds[2]);
        bounds[3] = max_reg(bounds[3], itemBounds[3]);
      }
    }

    return bounds;
  } else {
    List xy = item["xy"];
    NumericVector x = as<NumericVector>(xy["x"]);
    NumericVector y = as<NumericVector>(xy["y"]);

    if (naRm) {
      x = x[!is_na(x)];
      y = y[!is_na(y)];
    }

    return NumericVector::create(min(x), min(y), max(x), max(y));
  }
}

class EnvelopeOperator: public GeovctrsOperator {
public:
  NumericVector xmin;
  NumericVector ymin;
  NumericVector xmax;
  NumericVector ymax;
  IntegerVector srid;
  bool naRm;

  EnvelopeOperator(bool naRm) {
    this->naRm = naRm;
  }

  void init(GEOSContextHandle_t context, size_t size) {
    NumericVector xmin(size);
    NumericVector ymin(size);
    NumericVector xmax(size);
    NumericVector ymax(size);
    IntegerVector srid(size);
    this->xmin = xmin;
    this->ymin = ymin;
    this->xmax = xmax;
    this->ymax = ymax;
    this->srid = srid;
  }

  void operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    double xmin1, ymin1, xmax1, ymax1;
    int srid;

    if (geometry == NULL && this->naRm) {
      xmin1 = R_PosInf;
      ymin1 = R_PosInf;
      xmax1 = R_NegInf;
      ymax1 = R_NegInf;
      srid = NA_INTEGER;
    } else if(geometry == NULL) {
      xmin1 = NA_REAL;
      ymin1 = NA_REAL;
      xmax1 = NA_REAL;
      ymax1 = NA_REAL;
      srid = NA_INTEGER;
    } else if (GEOSisEmpty_r(context, geometry)) {
      xmin1 = R_PosInf;
      ymin1 = R_PosInf;
      xmax1 = R_NegInf;
      ymax1 = R_NegInf;
      srid = GEOSGetSRID_r(context, geometry);
    } else {
      List coords = GeovctrsFeatureFactory::getFeature(context, geometry);
      NumericVector bounds = bounds_from_coords(coords, this->naRm);

      xmin1 = bounds[0];
      ymin1 = bounds[1];
      xmax1 = bounds[2];
      ymax1 = bounds[3];
      srid = GEOSGetSRID_r(context, geometry);
    }

    this->xmin[i] = xmin1;
    this->ymin[i] = ymin1;
    this->xmax[i] = xmax1;
    this->ymax[i] = ymax1;
    this->srid[i] = srid;
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newRect(this->xmin, this->ymin, this->xmax, this->ymax, this->srid);
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_envelope(SEXP data, bool naRm) {
  EnvelopeOperator op(naRm);
  op.initProvider(data);
  return op.operate();
}

class RangeOperator: public GeovctrsRecursiveOperator {
public:
  bool naRm;
  double xmin;
  double ymin;
  double zmin;
  double xmax;
  double ymax;
  double zmax;
  int srid;

  RangeOperator(bool naRm) {
    this->naRm = naRm;
  }

  void reset() {
    this->xmin = R_PosInf;
    this->ymin = R_PosInf;
    this->zmin = R_PosInf;
    this->xmax = R_NegInf;
    this->ymax = R_NegInf;
    this->zmax = R_NegInf;
    this->srid = NA_INTEGER;
  }

  void nextCoordinate(GEOSContextHandle_t context, double x, double y, double z) {
    if (naRm) {
      this->xmin = min_na_rm(this->xmin, x);
      this->ymin = min_na_rm(this->ymin, y);
      this->zmin = min_na_rm(this->zmin, z);
      this->xmax = max_na_rm(this->xmax, x);
      this->ymax = max_na_rm(this->ymax, y);
      this->zmax = max_na_rm(this->zmax, z);
    } else {
      this->xmin = min_reg(this->xmin, x);
      this->ymin = min_reg(this->ymin, y);
      this->zmin = min_reg(this->zmin, z);
      this->xmax = max_reg(this->xmax, x);
      this->ymax = max_reg(this->ymax, y);
      this->zmax = max_reg(this->zmax, z);
    }
  }
};

class BboxOperator: public RangeOperator {
public:
  BboxOperator(bool naRm): RangeOperator(naRm) {
    this->naRm = naRm;
    this->reset();
  }

  virtual void operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    GeovctrsRecursiveOperator::operateNext(context, geometry, i);
    if (geometry != NULL) {
      int featureSRID = GEOSGetSRID_r(context, geometry);
      if (IntegerVector::is_na(this->srid)) {
        this->srid = featureSRID;
      } else if(this->srid != featureSRID) {
        stop("Can't compute ranges for a vector with more than one SRID");
      }
    }
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newRect(
      NumericVector::create(this->xmin),
      NumericVector::create(this->ymin),
      NumericVector::create(this->xmax),
      NumericVector::create(this->ymax),
      IntegerVector::create(this->srid)
    );
  }
};


// [[Rcpp::export]]
SEXP geovctrs_cpp_bbox(SEXP data, bool naRm) {
  BboxOperator op(naRm);
  op.initProvider(data);
  return op.operate();
}
