
#ifndef GEOVCTRS_GEOS_PROVIDER_HPP
#define GEOVCTRS_GEOS_PROVIDER_HPP

#include <geos_c.h>
#include "geometry-factory.hpp"
#include "geovctrs/factory.hpp"
#include <memory.h>
#include <Rcpp.h>
using namespace Rcpp;

// ---- base ----

class GeovctrsGEOSProvider {
public:
  virtual void init(GEOSContextHandle_t context) {}
  virtual GEOSGeometry* getNext(GEOSContextHandle_t context, R_xlen_t i) = 0;
  virtual void destroyNext(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    if (geometry != NULL) {
      GEOSGeom_destroy_r(context, geometry);
    }
  }
  virtual void finish(GEOSContextHandle_t context) {}
  virtual R_xlen_t size() = 0;
  virtual ~GeovctrsGEOSProvider() {}
};



class GeovctrsGEOSConstantProvider: public GeovctrsGEOSProvider {
public:
  std::unique_ptr<GeovctrsGEOSProvider> baseProvider;
  GEOSGeometry* geometry;
  bool hasFirst;

  GeovctrsGEOSConstantProvider(GeovctrsGEOSProvider* baseProvider) {
    this->baseProvider = std::unique_ptr<GeovctrsGEOSProvider> { baseProvider };
    this->hasFirst = false;
    this->geometry = NULL;
  }

  void init(GEOSContextHandle_t context) {
    this->baseProvider->init(context);
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, R_xlen_t i) {
    if (!this->hasFirst) {
      this->geometry = this->baseProvider->getNext(context, i);
      this->hasFirst = true;
    }
    return this->geometry;
  }

  void destroyNext(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    // this gets called at every iteration, but we need
    // the geometry to exist for all of them!
  }

  void finish(GEOSContextHandle_t context) {
    if (this->baseProvider) {
      this->baseProvider->destroyNext(context, this->geometry);
      this->baseProvider->finish(context);
    }
  }

  R_xlen_t size() {
    return 1;
  }
};



class GeovctrsGEOSWKTProvider: public GeovctrsGEOSProvider {
public:
  CharacterVector data;
  GEOSWKTReader *wktReader;

  GeovctrsGEOSWKTProvider(CharacterVector data) {
    this->data = data;
    this->wktReader = NULL;
  }

  void init(GEOSContextHandle_t context) {
    this->wktReader = GEOSWKTReader_create_r(context);
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, R_xlen_t i) {
    GEOSGeometry* geometry;
    if (CharacterVector::is_na(this->data[i])) {
      geometry = NULL;
    } else {
      geometry = GEOSWKTReader_read_r(
        context,
        this->wktReader,
        this->data[i]
      );
    }

    return geometry;
  }

  void finish(GEOSContextHandle_t context) {
    if (this->wktReader != NULL) {
      GEOSWKTReader_destroy_r(context, this->wktReader);
      this->wktReader = NULL;
    }
  }

  R_xlen_t size() {
    return this->data.size();
  }
};



class GeovctrsGEOSWKBProvider: public GeovctrsGEOSProvider {
public:
  List data;
  GEOSWKBReader *wkbReader;

  GeovctrsGEOSWKBProvider(List data) {
    this->data = data;
    this->wkbReader = NULL;
  }

  void init(GEOSContextHandle_t context) {
    this->wkbReader = GEOSWKBReader_create_r(context);
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, R_xlen_t i) {
    GEOSGeometry* geometry;
    if (this->data[i] == R_NilValue) {
      geometry = NULL;
    } else {
      RawVector r = this->data[i];
      geometry = GEOSWKBReader_read_r(context, this->wkbReader, &(r[0]), r.size());
    }

    return geometry;
  }

  void finish(GEOSContextHandle_t context) {
    if (this->wkbReader != NULL) {
      GEOSWKBReader_destroy_r(context, this->wkbReader);
      this->wkbReader = NULL;
    }
  }

  R_xlen_t size() {
    return this->data.size();
  }
};



class GeovctrsGEOSCollectionProvider: public GeovctrsGEOSProvider {
public:
  List features;
  IntegerVector srid;

  GeovctrsGEOSCollectionProvider(List data) {
    this->features = data["feature"];
    this->srid = data["srid"];
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, R_xlen_t i) {
    GEOSGeometry* geometry;
    if (this->features[i] == R_NilValue) {
      geometry = NULL;
    } else {
      geometry = GeovctrsGEOSGeometryFactory::getFeature(context, this->features[i]);
      GEOSSetSRID_r(context, geometry, this->srid[i]);
    }

    return geometry;
  }

  R_xlen_t size() {
    return this->features.size();
  }
};



class GeovctrsGEOSXYProvider: public GeovctrsGEOSProvider {
public:
  NumericVector x;
  NumericVector y;

  GeovctrsGEOSXYProvider(List xy) {
    this->x = xy["x"];
    this->y = xy["y"];
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, R_xlen_t i) {
    GEOSGeometry* geometry;

    if (NumericVector::is_na(this->x[i]) && NumericVector::is_na(this->y[i])) {
      geometry = GEOSGeom_createEmptyPoint_r(context);
    } else {
      GEOSCoordSequence* seq = GEOSCoordSeq_create_r(context, 1, 2);
      GEOSCoordSeq_setX_r(context, seq, 0, this->x[i]);
      GEOSCoordSeq_setY_r(context, seq, 0, this->y[i]);

      geometry = GEOSGeom_createPoint_r(context, seq);
    }

    return geometry;
  }

  R_xlen_t size() {
    return this->x.size();
  }
};


class GeovctrsGEOSXYZProvider: public GeovctrsGEOSXYProvider {
public:
  NumericVector z;

  GeovctrsGEOSXYZProvider(List xy): GeovctrsGEOSXYProvider(xy) {
    this->z = xy["z"];
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, R_xlen_t i) {
    GEOSGeometry* geometry;

    if (NumericVector::is_na(this->x[i]) &&
        NumericVector::is_na(this->y[i]) &&
        NumericVector::is_na(this->z[i])) {
      geometry = GEOSGeom_createEmptyPoint_r(context);
    } else {
      GEOSCoordSequence* seq = GEOSCoordSeq_create_r(context, 1, 3);
      GEOSCoordSeq_setX_r(context, seq, 0, this->x[i]);
      GEOSCoordSeq_setY_r(context, seq, 0, this->y[i]);
      GEOSCoordSeq_setZ_r(context, seq, 0, this->z[i]);

      geometry = GEOSGeom_createPoint_r(context, seq);
    }

    return geometry;
  }
};


class GeovctrsGEOSSegmentProvider: public GeovctrsGEOSProvider {
public:
  NumericVector x0;
  NumericVector y0;
  NumericVector x1;
  NumericVector y1;
  IntegerVector srid;

  GeovctrsGEOSSegmentProvider(List segment) {
    this->x0 = segment["x0"];
    this->y0 = segment["y0"];
    this->x1 = segment["x1"];
    this->y1 = segment["y1"];
    this->srid = segment["srid"];
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, R_xlen_t i) {
    double x0, y0, x1, y1;
    int srid;
    GEOSGeometry* geometry;

    x0 = this->x0[i];
    y0 = this->y0[i];
    x1 = this->x1[i];
    y1 = this->y1[i];
    srid = this->srid[i];

    if (NumericVector::is_na(x0) &&
        NumericVector::is_na(y0) &&
        NumericVector::is_na(x1) &&
        NumericVector::is_na(y1) &&
        IntegerVector::is_na(srid)) {
      geometry = NULL;
    } else if(NumericVector::is_na(x0) &&
      NumericVector::is_na(y0) &&
      NumericVector::is_na(x1) &&
      NumericVector::is_na(y1)) {
      geometry = GEOSGeom_createEmptyLineString_r(context);
      GEOSSetSRID_r(context, geometry, srid);
    } else {
      GEOSCoordSequence* seq = GEOSCoordSeq_create_r(context, 2, 2);

      // start
      GEOSCoordSeq_setX_r(context, seq, 0, this->x0[i]);
      GEOSCoordSeq_setY_r(context, seq, 0, this->y0[i]);

      // end
      GEOSCoordSeq_setX_r(context, seq, 1, this->x1[i]);
      GEOSCoordSeq_setY_r(context, seq, 1, this->y1[i]);

      geometry = GEOSGeom_createLineString_r(context, seq);
      GEOSSetSRID_r(context, geometry, this->srid[i]);
    }

    return geometry;
  }

  R_xlen_t size() {
    return this->x0.size();
  }
};



class GeovctrsGEOSRectProvider: public GeovctrsGEOSProvider {
public:
  NumericVector xmin;
  NumericVector ymin;
  NumericVector xmax;
  NumericVector ymax;
  IntegerVector srid;

  GeovctrsGEOSRectProvider(List rect) {
    this->xmin = rect["xmin"];
    this->ymin = rect["ymin"];
    this->xmax = rect["xmax"];
    this->ymax = rect["ymax"];
    this->srid = rect["srid"];
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, R_xlen_t i) {
    double xmin1, ymin1, xmax1, ymax1;
    int srid;
    GEOSGeometry* geometry;

    xmin1 = this->xmin[i];
    ymin1 = this->ymin[i];
    xmax1 = this->xmax[i];
    ymax1 = this->ymax[i];
    srid = this->srid[i];

    if (NumericVector::is_na(xmin1) &&
        NumericVector::is_na(ymin1) &&
        NumericVector::is_na(xmax1) &&
        NumericVector::is_na(ymax1) &&
        IntegerVector::is_na(srid)) {
      geometry = NULL;
    } else if(NumericVector::is_na(xmin1) ||
      NumericVector::is_na(ymin1) ||
      NumericVector::is_na(xmax1) ||
      NumericVector::is_na(ymax1)) {
      geometry = GEOSGeom_createEmptyPolygon_r(context);
      GEOSSetSRID_r(context, geometry, srid);
    } else {
      // counter clockwise!
      GEOSCoordSequence* seq = GEOSCoordSeq_create_r(context, 5, 2);
      GEOSCoordSeq_setX_r(context, seq, 0, xmin1); GEOSCoordSeq_setY_r(context, seq, 0, ymin1);
      GEOSCoordSeq_setX_r(context, seq, 1, xmax1); GEOSCoordSeq_setY_r(context, seq, 1, ymin1);
      GEOSCoordSeq_setX_r(context, seq, 2, xmax1); GEOSCoordSeq_setY_r(context, seq, 2, ymax1);
      GEOSCoordSeq_setX_r(context, seq, 3, xmin1); GEOSCoordSeq_setY_r(context, seq, 3, ymax1);
      GEOSCoordSeq_setX_r(context, seq, 4, xmin1); GEOSCoordSeq_setY_r(context, seq, 4, ymin1);

      GEOSGeometry* shell = GEOSGeom_createLinearRing_r(context, seq);
      geometry = GEOSGeom_createPolygon_r(context, shell, NULL, 0);
      GEOSSetSRID_r(context, geometry, srid);
    }

    return geometry;
  }

  R_xlen_t size() {
    return this->xmin.size();
  }
};

#endif
