
#include "geovctrs/geos/operator.hpp"
#include "geovctrs/factory.hpp"
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
  } else if (x1NA) {
    return x2i;
  } else if (x2NA) {
    return x1i;
  } else {
    return std::min(x1i, x2i);
  }
}

double max_na_rm(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i);
  bool x2NA = NumericVector::is_na(x2i);
  if (x1NA && x2NA) {
    return R_NegInf;
  } else if (x1NA) {
    return x2i;
  } else if (x2NA) {
    return x1i;
  } else {
    return std::max(x1i, x2i);
  }
}

double min_finite(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i) || x1i == R_NegInf || x1i == R_PosInf;
  bool x2NA = NumericVector::is_na(x2i) || x2i == R_NegInf || x2i == R_PosInf;
  if (x1NA && x2NA) {
    return R_PosInf;
  } else if (x1NA) {
    return x2i;
  } else if (x2NA) {
    return x1i;
  } else {
    return std::min(x1i, x2i);
  }
}

double max_finite(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i) || x1i == R_NegInf || x1i == R_PosInf;
  bool x2NA = NumericVector::is_na(x2i) || x2i == R_NegInf || x2i == R_PosInf;
  if (x1NA && x2NA) {
    return R_NegInf;
  } else if (x1NA) {
    return x2i;
  } else if (x2NA) {
    return x1i;
  } else {
    return std::max(x1i, x2i);
  }
}

// [[Rcpp::export]]
NumericVector pmin2(NumericVector x1, NumericVector x2)  {
  NumericVector out (x1.size());

  for (R_xlen_t i=0; i<out.size(); i++) {
    out[i] = min_na_rm(x1[i], x2[i]);
  }

  return out;
}

// [[Rcpp::export]]
NumericVector pmax2(NumericVector x1, NumericVector x2)  {
  NumericVector out (x1.size());

  for (R_xlen_t i=0; i<out.size(); i++) {
    out[i] = max_na_rm(x1[i], x2[i]);
  }

  return out;
}

class RangeOperator: public GeovctrsGEOSRecursiveOperator {
public:
  bool naRm;
  bool onlyFinite;
  double xmin;
  double ymin;
  double zmin;
  double xmax;
  double ymax;
  double zmax;

  RangeOperator(bool naRm, bool onlyFinite) {
    this->naRm = naRm;
    this->onlyFinite = onlyFinite;
  }

  void reset() {
    this->xmin = R_PosInf;
    this->ymin = R_PosInf;
    this->zmin = R_PosInf;
    this->xmax = R_NegInf;
    this->ymax = R_NegInf;
    this->zmax = R_NegInf;
  }

  void nextXYZ(GEOSContextHandle_t context, double x, double y, double z) {
    if (onlyFinite) {
      this->xmin = min_finite(this->xmin, x);
      this->ymin = min_finite(this->ymin, y);
      this->zmin = min_finite(this->zmin, z);
      this->xmax = max_finite(this->xmax, x);
      this->ymax = max_finite(this->ymax, y);
      this->zmax = max_finite(this->zmax, z);
    } else if (naRm) {
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
  int srid;

  BboxOperator(bool naRm, bool onlyFinite): RangeOperator(naRm, onlyFinite) {
    this->srid = NA_INTEGER;
    this->reset();
  }

  virtual void nextFeature(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    GeovctrsGEOSRecursiveOperator::nextFeature(context, geometry, i);
    if (geometry != NULL) {
      int featureSRID = GEOSGetSRID_r(context, geometry);
      if (IntegerVector::is_na(this->srid)) {
        this->srid = featureSRID;
      } else if(this->srid != featureSRID) {
        stop("Can't compute ranges for a vector with more than one SRID");
      }
    }
  }

  virtual SEXP assemble(GEOSContextHandle_t context) {
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
SEXP geovctrs_cpp_bbox(SEXP data, bool naRm, bool onlyFinite) {
  BboxOperator op(naRm, onlyFinite);
  op.initProvider(data);
  return op.operate();
}


class ZLimOperator: public BboxOperator {
public:
  ZLimOperator(bool naRm, bool onlyFinite): BboxOperator(naRm, onlyFinite) {}

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newLim(
      NumericVector::create(this->zmin),
      NumericVector::create(this->zmax)
    );
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_z_range(SEXP data, bool naRm, bool onlyFinite) {
  ZLimOperator op(naRm, onlyFinite);
  op.initProvider(data);
  return op.operate();
}


class EnvelopeOperator: public RangeOperator {
public:
  NumericVector xminVec;
  NumericVector yminVec;
  NumericVector zminVec;
  NumericVector xmaxVec;
  NumericVector ymaxVec;
  NumericVector zmaxVec;
  IntegerVector sridVec;

  EnvelopeOperator(bool naRm, bool onlyFinite): RangeOperator(naRm, onlyFinite) {
    this->reset();
  }

  void init(GEOSContextHandle_t context, R_xlen_t size) {
    this->xminVec = NumericVector(size);
    this->yminVec = NumericVector(size);
    this->zminVec = NumericVector(size);
    this->xmaxVec = NumericVector(size);
    this->ymaxVec = NumericVector(size);
    this->zmaxVec = NumericVector(size);
    this->sridVec = IntegerVector(size);
  }

  void nextFeature(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    int featureSRID;

    if (geometry == NULL && this->naRm) {
      this->xmin = R_PosInf;
      this->ymin = R_PosInf;
      this->zmin = R_PosInf;
      this->xmax = R_NegInf;
      this->ymax = R_NegInf;
      this->zmax = R_NegInf;
      featureSRID = NA_INTEGER;
    } else if(geometry == NULL) {
      this->xmin = NA_REAL;
      this->ymin = NA_REAL;
      this->zmin = NA_REAL;
      this->xmax = NA_REAL;
      this->ymax = NA_REAL;
      this->zmax = NA_REAL;
      featureSRID = NA_INTEGER;
    } else {
      this->reset();
      this->nextGeometry(context, geometry);
      featureSRID = GEOSGetSRID_r(context, geometry);
    }

    this->xminVec[i] = this->xmin;
    this->yminVec[i] = this->ymin;
    this->zminVec[i] = this->zmin;
    this->xmaxVec[i] = this->xmax;
    this->ymaxVec[i] = this->ymax;
    this->zmaxVec[i] = this->zmax;
    this->sridVec[i] = featureSRID;
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newRect(
      this->xminVec,
      this->yminVec,
      this->xmaxVec,
      this->ymaxVec,
      this->sridVec
    );
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_envelope(SEXP data, bool naRm, bool onlyFinite) {
  EnvelopeOperator op(naRm, onlyFinite);
  op.initProvider(data);
  return op.operate();
}


class ZEnvelopeOperator: public EnvelopeOperator {
public:
  ZEnvelopeOperator(bool naRm, bool onlyFinite): EnvelopeOperator(naRm, onlyFinite) {}

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newLim(this->zminVec, this->zmaxVec);
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_z_envelope(SEXP data, bool naRm, bool onlyFinite) {
  ZEnvelopeOperator op(naRm, onlyFinite);
  op.initProvider(data);
  return op.operate();
}
