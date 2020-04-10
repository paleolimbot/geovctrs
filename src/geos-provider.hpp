
#ifndef GEOS_PROVIDER_H
#define GEOS_PROVIDER_H

#include <geos_c.h>
#include "geovctrs/feature-factory.hpp"
#include "geovctrs/geos-feature-factory.hpp"
#include "geovctrs/factory.hpp"
#include <memory.h>
#include <Rcpp.h>
using namespace Rcpp;

// ---- base ----

class GeometryProvider {
public:
  virtual void init(GEOSContextHandle_t context) {}
  virtual GEOSGeometry* getNext(GEOSContextHandle_t context, size_t i) = 0;
  virtual void finish(GEOSContextHandle_t context) {}
  virtual size_t size() = 0;
  virtual ~GeometryProvider() {}
};

class GeometryExporter {
public:
  virtual void init(GEOSContextHandle_t context, size_t size) {}
  virtual void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) = 0;
  virtual SEXP assemble(GEOSContextHandle_t context) {
    return R_NilValue;
  }
  virtual void finish(GEOSContextHandle_t context) {}
  virtual ~GeometryExporter() {}
};

class ConstantGeometryProvider: public GeometryProvider {
public:
  std::unique_ptr<GeometryProvider> baseProvider;
  GEOSGeometry* geometry;
  bool hasFirst;

  ConstantGeometryProvider(GeometryProvider* baseProvider) {
    this->baseProvider = std::unique_ptr<GeometryProvider> { baseProvider };
    this->hasFirst = false;
  }

  void init(GEOSContextHandle_t context) {
    this->baseProvider->init(context);
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, size_t i) {
    if (!this->hasFirst) {
      this->geometry = this->baseProvider->getNext(context, i);
    }
    return this->geometry;
  }

  void finish(GEOSContextHandle_t context) {
    this->baseProvider->finish(context);
  }

  size_t size() {
    return 1;
  }
};

// ---- WKT ----

class WKTGeometryProvider: public GeometryProvider {
public:
  CharacterVector data;
  GEOSWKTReader *wktReader;

  WKTGeometryProvider(CharacterVector data) {
    this->data = data;
    this->wktReader = NULL;
  }

  void init(GEOSContextHandle_t context) {
    this->wktReader = GEOSWKTReader_create_r(context);
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, size_t i) {
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

  size_t size() {
    return this->data.size();
  }
};

class WKTGeometryExporter: public GeometryExporter {
public:
  CharacterVector data;
  bool trim;
  int precision;
  int dimensions;
  GEOSWKTWriter *wktWriter;

  WKTGeometryExporter(CharacterVector ptype) {
    this->trim = ptype.attr("trim");
    this->precision = ptype.attr("precision");
    this->dimensions = ptype.attr("dimensions");
    this->wktWriter = NULL;
  }

  void init(GEOSContextHandle_t context, size_t size) {
    this->wktWriter = GEOSWKTWriter_create_r(context);
    GEOSWKTWriter_setTrim_r(context, this->wktWriter, this->trim);
    GEOSWKTWriter_setRoundingPrecision_r(context, this->wktWriter, this->precision);
    GEOSWKTWriter_setOutputDimension_r(context, this->wktWriter, this->dimensions);

    CharacterVector data(size);
    this->data = data;
  }

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (geometry == NULL) {
      this->data[i] = NA_STRING;
    } else {
      std::string wkt_single;
      wkt_single = GEOSWKTWriter_write_r(context, wktWriter, geometry);
      this->data[i] = wkt_single;
    }
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newWKT(this->data);
  }

  void finish(GEOSContextHandle_t context) {
    if (this->wktWriter != NULL) {
      GEOSWKTWriter_destroy_r(context, this->wktWriter);
      this->wktWriter = NULL;
    }
  }
};

// ---- WKB -----

class WKBGeometryProvider: public GeometryProvider {
public:
  List data;
  GEOSWKBReader *wkbReader;

  WKBGeometryProvider(List data) {
    this->data = data;
    this->wkbReader = NULL;
  }

  void init(GEOSContextHandle_t context) {
    this->wkbReader = GEOSWKBReader_create_r(context);
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, size_t i) {
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

  size_t size() {
    return this->data.size();
  }
};

class WKBGeometryExporter: public GeometryExporter {
public:
  List data;
  GEOSWKBWriter *wkbWriter;
  int includeSRID;
  int dimensions;
  int endian;

  WKBGeometryExporter(List ptype) {
    this->includeSRID = ptype.attr("include_srid");
    this->dimensions = ptype.attr("dimensions");
    this->endian = ptype.attr("endian");
    this->wkbWriter = NULL;
  }

  void init(GEOSContextHandle_t context, size_t size) {
    this->wkbWriter = GEOSWKBWriter_create_r(context);
    if (!LogicalVector::is_na(this->includeSRID)) {
      GEOSWKBWriter_setIncludeSRID_r(context, this->wkbWriter, this->includeSRID);
    }
    GEOSWKBWriter_setOutputDimension_r(context, this->wkbWriter, this->dimensions);
    if (!IntegerVector::is_na(this->endian)) {
      GEOSWKBWriter_setByteOrder_r(context, this->wkbWriter, this->endian);
    }

    List data(size);
    this->data = data;
  }

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (geometry == NULL) {
      this->data[i] = R_NilValue;
    } else {
      if (IntegerVector::is_na(this->includeSRID)) {
        int srid = GEOSGetSRID_r(context, geometry);
        bool useSRID = (srid != 0) && !IntegerVector::is_na(srid);
        GEOSWKBWriter_setIncludeSRID_r(context, this->wkbWriter, useSRID);
      }

      // GEOSWKBWriter won't deal with POINT EMPTY, but we handle in the same way
      // as sf (GEOSWKBReader seems to have no problem with this solution)
      // TODO: handle SRID, multiple dimensions
      if (GEOSisEmpty_r(context, geometry) &&
          GEOSGeomTypeId_r(context, geometry) == GEOSGeomTypes::GEOS_POINT) {
        size_t size = 21;
        const unsigned char buf[] = {
          // little endian
          0x01,
          // geometry type: point 2D
          0x01, 0x00, 0x00, 0x00,
          // x coordinate
          0xa2, 0x07, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x7f,
          // y coordinate
          0xa2, 0x07, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x7f
        };
        RawVector raw(size);
        memcpy(&(raw[0]), buf, size);
        this->data[i] = raw;
      } else {

        size_t size;
        unsigned char *buf = GEOSWKBWriter_write_r(context, this->wkbWriter, geometry, &size);
        RawVector raw(size);
        memcpy(&(raw[0]), buf, size);
        GEOSFree_r(context, buf);

        this->data[i] = raw;
      }
    }
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newWKB(this->data);
  }

  void finish(GEOSContextHandle_t context) {
    if (this->wkbWriter != NULL) {
      GEOSWKBWriter_destroy_r(context, this->wkbWriter);
      this->wkbWriter = NULL;
    }
  }
};

// --- GeoCollection

class GeoCollectionProvider: public GeometryProvider {
public:
  List features;
  IntegerVector srid;

  GeoCollectionProvider(List data) {
    this->features = data["feature"];
    this->srid = data["srid"];
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, size_t i) {
    GEOSGeometry* geometry;
    if (this->features[i] == R_NilValue) {
      geometry = NULL;
    } else {
      geometry = GEOSFeatureFactory::getFeature(context, this->features[i]);
      GEOSSetSRID_r(context, geometry, this->srid[i]);
    }

    return geometry;
  }

  size_t size() {
    return this->features.size();
  }
};

class GeoCollectionExporter: public GeometryExporter {
public:
  List data;
  IntegerVector srid;

  void init(GEOSContextHandle_t context, size_t size) {
    NumericVector data(size);
    IntegerVector srid(size);
    this->data = data;
    this->srid = srid;
  }

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (geometry == NULL) {
      this->data[i] = R_NilValue;
      this->srid[i] = NA_INTEGER;
    } else {
      this->data[i] = GeovctrsFeatureFactory::getFeature(context, geometry);
      this->srid[i] = GEOSGetSRID_r(context, geometry);
    }
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newCollection(this->data, this->srid);
  }
};

// --- XY

class XYProvider: public GeometryProvider {
public:
  NumericVector x;
  NumericVector y;

  XYProvider(List xy) {
    this->x = xy["x"];
    this->y = xy["y"];
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, size_t i) {
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

  size_t size() {
    return this->x.size();
  }
};

class XYExporter: public GeometryExporter {
public:
  NumericVector x;
  NumericVector y;

  void init(GEOSContextHandle_t context, size_t size) {
    NumericVector x(size);
    NumericVector y(size);
    this->x = x;
    this->y = y;
  }

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    double x, y;
    if (geometry == NULL) {
      x = NA_REAL;
      y = NA_REAL;
    } else {
      if (GEOSGeomTypeId_r(context, geometry) != GEOSGeomTypes::GEOS_POINT) {
        stop("Can't represent a non-point as a geo_xy()");
      }

      if (GEOSGetSRID_r(context, geometry) != 0) {
        Function warning("warning");
        warning("Dropping SRID in cast to geo_xy()");
      }

      // geos doesn't differentiate between POINT (nan, nan) and POINT EMPTY
      if (GEOSisEmpty_r(context, geometry)) {
        x = NA_REAL;
        y = NA_REAL;
      } else {
        GEOSGeomGetX_r(context, geometry, &x);
        GEOSGeomGetY_r(context, geometry, &y);
      }
    }

    this->x[i] = x;
    this->y[i] = y;
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newXY(this->x, this->y);
  }
};

// --- segment ----

class SegmentProvider: public GeometryProvider {
public:
  NumericVector x0;
  NumericVector y0;
  NumericVector x1;
  NumericVector y1;
  IntegerVector srid;

  SegmentProvider(List segment) {
    List start = segment["start"];
    List end = segment["end"];
    this->x0 = start["x"];
    this->y0 = start["y"];
    this->x1 = end["x"];
    this->y1 = end["y"];
    this->srid = segment["srid"];
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, size_t i) {
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

  size_t size() {
    return this->x0.size();
  }
};

class SegmentExporter: public GeometryExporter {
public:
  NumericVector x0;
  NumericVector y0;
  NumericVector x1;
  NumericVector y1;
  IntegerVector srid;

  void init(GEOSContextHandle_t context, size_t size) {
    NumericVector x0(size);
    NumericVector y0(size);
    NumericVector x1(size);
    NumericVector y1(size);
    IntegerVector srid(size);
    this->x0 = x0;
    this->y0 = y0;
    this->x1 = x1;
    this->y1 = y1;
    this->srid = srid;
  }

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    double x0, y0, x1, y1;
    int srid;

    if (geometry == NULL) {
      x0 = NA_REAL;
      y0 = NA_REAL;
      x1 = NA_REAL;
      y1 = NA_REAL;
      srid = NA_INTEGER;
    } else {
      if (GEOSGeomTypeId_r(context, geometry) != GEOSGeomTypes::GEOS_LINESTRING) {
        stop("Can't represent a non-linestring as a geo_segment()");
      }

      if (!GEOSisEmpty_r(context, geometry) && GEOSGeomGetNumPoints_r(context, geometry) != 2) {
        stop("linestrings must have exactly two points to be represented as a geo_segment()");
      }



      if (GEOSisEmpty_r(context, geometry)) {
        x0 = NA_REAL;
        y0 = NA_REAL;
        x1 = NA_REAL;
        y1 = NA_REAL;
      } else {
        const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(context, geometry);
        GEOSCoordSeq_getX_r(context, seq, 0, &x0);
        GEOSCoordSeq_getY_r(context, seq, 0, &y0);
        GEOSCoordSeq_getX_r(context, seq, 1, &x1);
        GEOSCoordSeq_getY_r(context, seq, 1, &y1);
      }

      srid = GEOSGetSRID_r(context, geometry);
    }

    this->x0[i] = x0;
    this->y0[i] = y0;
    this->x1[i] = x1;
    this->y1[i] = y1;
    this->srid[i] = srid;
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newSegment(this->x0, this->y0, this->x1, this->y1, this->srid);
  }
};

// --- rect ----

class GeoRectProvider: public GeometryProvider {
public:
  NumericVector xmin;
  NumericVector ymin;
  NumericVector xmax;
  NumericVector ymax;
  IntegerVector srid;

  GeoRectProvider(List rect) {
    this->xmin = rect["xmin"];
    this->ymin = rect["ymin"];
    this->xmax = rect["xmax"];
    this->ymax = rect["ymax"];
    this->srid = rect["srid"];
  }

  GEOSGeometry* getNext(GEOSContextHandle_t context, size_t i) {
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
      GEOSGeometry* holes[0];
      geometry = GEOSGeom_createPolygon_r(context, shell, holes, 0);
      GEOSSetSRID_r(context, geometry, srid);
    }

    return geometry;
  }

  size_t size() {
    return this->xmin.size();
  }
};

#endif
