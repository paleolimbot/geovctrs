
#include "geos-operator.h"
#include "geos-coords.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pmin2(NumericVector x1, NumericVector x2)  {
  NumericVector out (x1.size());
  double x1i, x2i;
  bool x1NA, x2NA;

  for (size_t i=0; i<out.size(); i++) {
    x1i = x1[i];
    x2i = x2[i];
    x1NA = NumericVector::is_na(x1i);
    x2NA = NumericVector::is_na(x2i);
    if (x1NA && x2NA) {
      out[i] = R_PosInf;
    } else if (x1NA || x2NA) {
      out[i] = NA_REAL;
    } else {
      out[i] = std::min(x1i, x2i);
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericVector pmax2(NumericVector x1, NumericVector x2)  {
  NumericVector out (x1.size());
  double x1i, x2i;
  bool x1NA, x2NA;

  for (size_t i=0; i<out.size(); i++) {
    x1i = x1[i];
    x2i = x2[i];
    x1NA = NumericVector::is_na(x1i);
    x2NA = NumericVector::is_na(x2i);
    if (x1NA && x2NA) {
      out[i] = R_NegInf;
    } else if (x1NA || x2NA) {
      out[i] = NA_REAL;
    } else {
      out[i] = std::max(x1i, x2i);
    }
  }

  return out;
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
      List xy = coords["xy"];
      NumericVector x = as<NumericVector>(xy["x"]);
      NumericVector y = as<NumericVector>(xy["y"]);

      if (this->naRm) {
        x = x[!is_na(x)];
        y = y[!is_na(y)];
      }

      xmin1 = min(x);
      ymin1 = min(y);
      xmax1 = max(x);
      ymax1 = max(y);
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
    result.attr("class") = CharacterVector::create("geo_rect", "geovctr", "vctrs_rcrd", "vctrs_vctr");
    return result;
  }
};

// [[Rcpp::export]]
SEXP cpp_envelope(SEXP data, bool naRm) {
  EnvelopeOperator op(naRm);
  op.initProvider(data);
  return op.operate();
}
