
#include "geos-operator.h"
#include "geos-coords.h"
using namespace Rcpp;

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

class EnvelopeOperator: public UnaryOperator {
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

  void init() {
    size_t size = this->provider->size();
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

  void operateNext(GEOSGeometry* geometry) {
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
    } else if (GEOSisEmpty_r(this->context, geometry)) {
      xmin1 = R_PosInf;
      ymin1 = R_PosInf;
      xmax1 = R_NegInf;
      ymax1 = R_NegInf;
      srid = GEOSGetSRID_r(this->context, geometry);
    } else {
      List coords = geometry_to_geo_coord(context, geometry);
      NumericVector bounds = bounds_from_coords(coords, this->naRm);

      xmin1 = bounds[0];
      ymin1 = bounds[1];
      xmax1 = bounds[2];
      ymax1 = bounds[3];
      srid = GEOSGetSRID_r(this->context, geometry);
    }

    this->xmin[this->counter] = xmin1;
    this->ymin[this->counter] = ymin1;
    this->xmax[this->counter] = xmax1;
    this->ymax[this->counter] = ymax1;
    this->srid[this->counter] = srid;
  }

  SEXP assemble() {
    List result = List::create(
      _["xmin"] = this->xmin,
      _["ymin"] = this->ymin,
      _["xmax"] = this->xmax,
      _["ymax"] = this->ymax,
      _["srid"] = this->srid
    );
    result.attr("class") = CharacterVector::create("geovctrs_rect", "geovctr", "vctrs_rcrd", "vctrs_vctr");
    return result;
  }
};

// [[Rcpp::export]]
SEXP cpp_envelope(SEXP data, bool naRm) {
  EnvelopeOperator op(naRm);
  op.initProvider(data);
  return op.operate();
}
