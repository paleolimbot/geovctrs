
#ifndef GEOS_PROVIDER_H
#define GEOS_PROVIDER_H

#include "geos-base.h"
#include "geos-coords-write.h"
#include "geos-coords.h"
#include <memory.h>
#include <Rcpp.h>
using namespace Rcpp;

// ---- base ----

class GeometryProvider {
public:
  GEOSContextHandle_t context;
  size_t recycleSize;

  virtual void init(GEOSContextHandle_t context) {
    this->context = context;
  }

  virtual GEOSGeometry* getNext() = 0;

  virtual void finish() {

  }

  virtual size_t size() = 0;

  virtual ~GeometryProvider() {

  }
};

class GeometryExporter {
public:
  GEOSContextHandle_t context;

  virtual void init(GEOSContextHandle_t context, size_t size) {
    this->context = context;
  }

  virtual void putNext(GEOSGeometry* geometry) = 0;

  virtual SEXP finish() {
    return R_NilValue;
  }

  virtual ~GeometryExporter() {

  }
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
    this->context = context;
    this->baseProvider->init(context);
  }

  GEOSGeometry* getNext() {
    if (!this->hasFirst) {
      this->geometry = this->baseProvider->getNext();
    }
    return this->geometry;
  }

  void finish() {
    this->baseProvider->finish();
  }

  size_t size() {
    return 1;
  }
};

// ---- WKT ----

class WKTGeometryProvider: public GeometryProvider {
public:
  CharacterVector data;
  GEOSWKTReader *wkt_reader;
  size_t counter;

  WKTGeometryProvider(CharacterVector data) {
    this->data = data;
    this->counter = 0;
  }

  void init(GEOSContextHandle_t context) {
    this->context = context;
    this->wkt_reader = GEOSWKTReader_create_r(context);
  }

  GEOSGeometry* getNext() {
    GEOSGeometry* geometry;
    if (CharacterVector::is_na(this->data[this->counter])) {
      geometry = NULL;
    } else {
      geometry = GEOSWKTReader_read_r(
        this->context,
        this->wkt_reader,
        this->data[this->counter]
      );
    }

    this->counter = this->counter + 1;
    return geometry;
  }

  void finish() {
    GEOSWKTReader_destroy_r(this->context, this->wkt_reader);
  }

  size_t size() {
    return (this->data).size();
  }
};

class WKTGeometryExporter: public GeometryExporter {
public:
  CharacterVector data;
  bool trim;
  int precision;
  int dimensions;
  GEOSWKTWriter *wkt_writer;
  size_t counter;

  WKTGeometryExporter(CharacterVector ptype) {
    this->trim = ptype.attr("trim");
    this->precision = ptype.attr("precision");
    this->dimensions = ptype.attr("dimensions");
    this->counter = 0;
  }

  void init(GEOSContextHandle_t context, size_t size) {
    this->context = context;

    this->wkt_writer = GEOSWKTWriter_create_r(context);
    GEOSWKTWriter_setTrim_r(this->context, this->wkt_writer, this->trim);
    GEOSWKTWriter_setRoundingPrecision_r(this->context, this->wkt_writer, this->precision);
    GEOSWKTWriter_setOutputDimension_r(this->context, this->wkt_writer, this->dimensions);

    CharacterVector data(size);
    data.attr("class") = CharacterVector::create("geovctrs_wkt", "geovctr", "vctrs_vctr");

    // set these to the defaults rather than the input values, as they aren't
    // used for reading (only writing)
    data.attr("trim") = true;
    data.attr("precision") =  IntegerVector::create(16);
    data.attr("dimensions") = IntegerVector::create(3);

    this->data = data;
  }

  void putNext(GEOSGeometry* geometry) {
    if (geometry == NULL) {
      this->data[this->counter] = NA_STRING;
    } else {
      std::string wkt_single;
      wkt_single = GEOSWKTWriter_write_r(this->context, wkt_writer, geometry);
      this->data[this->counter] = wkt_single;
    }

    this->counter = this->counter + 1;
  }

  SEXP finish() {
    GEOSWKTWriter_destroy_r(this->context, this->wkt_writer);
    return this->data;
  }
};

// ---- WKB -----

class WKBGeometryProvider: public GeometryProvider {
public:
  List data;
  GEOSWKBReader *wkb_reader;
  size_t counter;

  WKBGeometryProvider(List data) {
    this->data = data;
    this->counter = 0;
  }

  void init(GEOSContextHandle_t context) {
    this->context = context;
    this->wkb_reader = GEOSWKBReader_create_r(context);
  }

  GEOSGeometry* getNext() {
    GEOSGeometry* geometry;
    if (this->data[this->counter] == R_NilValue) {
      geometry = NULL;
    } else {
      RawVector r = this->data[this->counter];
      geometry = GEOSWKBReader_read_r(context, this->wkb_reader, &(r[0]), r.size());
    }

    this->counter = this->counter + 1;
    return geometry;
  }

  void finish() {
    GEOSWKBReader_destroy_r(this->context, this->wkb_reader);
  }

  size_t size() {
    return (this->data).size();
  }
};

class WKBGeometryExporter: public GeometryExporter {
public:
  List data;
  GEOSWKBWriter *wkb_writer;
  size_t counter;
  int includeSRID;
  int dimensions;
  int endian;

  WKBGeometryExporter(List ptype) {
    this->includeSRID = ptype.attr("include_srid");
    this->dimensions = ptype.attr("dimensions");
    this->endian = ptype.attr("endian");
    this->counter = 0;
  }

  void init(GEOSContextHandle_t context, size_t size) {
    this->context = context;
    this->wkb_writer = GEOSWKBWriter_create_r(context);
    if (!LogicalVector::is_na(this->includeSRID)) {
      GEOSWKBWriter_setIncludeSRID_r(this->context, this->wkb_writer, this->includeSRID);
    }
    GEOSWKBWriter_setOutputDimension_r(this->context, this->wkb_writer, this->dimensions);
    if (!IntegerVector::is_na(this->endian)) {
      GEOSWKBWriter_setByteOrder_r(this->context, this->wkb_writer, this->endian);
    }

    List data(size);
    data.attr("class") = CharacterVector::create("geovctrs_wkb", "geovctr", "vctrs_vctr");
    data.attr("include_srid") = LogicalVector::create(LogicalVector::get_na());
    data.attr("dimensions") = IntegerVector::create(3);
    data.attr("endian") = IntegerVector::create(LogicalVector::get_na());

    this->data = data;
  }

  void putNext(GEOSGeometry* geometry) {
    if (geometry == NULL) {
      this->data[this->counter] = R_NilValue;
    } else {
      if (IntegerVector::is_na(this->includeSRID)) {
        int srid = GEOSGetSRID_r(this->context, geometry);
        bool useSRID = (srid != 0) && !IntegerVector::is_na(srid);
        GEOSWKBWriter_setIncludeSRID_r(this->context, this->wkb_writer, useSRID);
      }

      // GEOSWKBWriter won't deal with POINT EMPTY, but we handle in the same way
      // as sf (GEOSWKBReader seems to have no problem with this solution)
      // TODO: handle SRID, multiple dimensions
      if (GEOSisEmpty_r(this->context, geometry) &&
          GEOSGeomTypeId_r(this->context, geometry) == GEOSGeomTypes::GEOS_POINT) {
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
        this->data[this->counter] = raw;
      } else {

        size_t size;
        unsigned char *buf = GEOSWKBWriter_write_r(this->context, this->wkb_writer, geometry, &size);
        RawVector raw(size);
        memcpy(&(raw[0]), buf, size);
        GEOSFree_r(this->context, buf);

        this->data[this->counter] = raw;
      }
    }

    this->counter = this->counter + 1;
  }

  SEXP finish() {
    GEOSWKBWriter_destroy_r(this->context, this->wkb_writer);
    return data;
  }
};

// --- GeoCollection

class GeoCollectionProvider: public GeometryProvider {
public:
  List features;
  IntegerVector srid;
  size_t counter;

  GeoCollectionProvider(List data) {
    this->features = data["feature"];
    this->srid = data["srid"];
  }

  void init(GEOSContextHandle_t context) {
    this->context = context;
    this->counter = 0;
  }

  GEOSGeometry* getNext() {
    GEOSGeometry* geometry;
    if (this->features[this->counter] == R_NilValue) {
      geometry = NULL;
    } else {
      geometry = feature_from_geo_coord(this->context, this->features[this->counter]);
      GEOSSetSRID_r(context, geometry, this->srid[this->counter]);
    }

    this->counter = this->counter + 1;
    return geometry;
  }

  size_t size() {
    return (this->features).size();
  }
};

class GeoCollectionExporter: public GeometryExporter {
public:
  List data;
  IntegerVector srid;
  size_t counter;

  void init(GEOSContextHandle_t context, size_t size) {
    IntegerVector srid(size);
    this->srid = srid;
    List data(size);
    this->data = data;
    this->context = context;
    this->counter = 0;
  }

  void putNext(GEOSGeometry* geometry) {
    if (geometry == NULL) {
      this->data[this->counter] = R_NilValue;
      this->srid[this->counter] = NA_INTEGER;
    } else {
      this->data[this->counter] = geometry_to_geo_coord(this->context, geometry);
      this->srid[this->counter] = GEOSGetSRID_r(this->context, geometry);
    }

    this->counter = this->counter + 1;
  }

  SEXP finish() {
    List out = List::create(_["feature"] = this->data, _["srid"] = this->srid);
    out.attr("class") = CharacterVector::create("geovctrs_collection", "geovctr", "vctrs_rcrd", "vctrs_vctr");
    return out;
  }
};

// --- XY

class XYProvider: public GeometryProvider {
public:
  NumericVector x;
  NumericVector y;
  size_t counter;

  XYProvider(List xy) {
    this->x = xy["x"];
    this->y = xy["y"];
  }

  void init(GEOSContextHandle_t context) {
    this->context = context;
    this->counter = 0;
  }

  GEOSGeometry* getNext() {
    GEOSGeometry* geometry;

    if (NumericVector::is_na(x[this->counter]) && NumericVector::is_na(y[this->counter])) {
      geometry = GEOSGeom_createEmptyPoint_r(this->context);
    } else {
      GEOSCoordSequence* seq = GEOSCoordSeq_create_r(this->context, 1, 2);
      GEOSCoordSeq_setX_r(this->context, seq, 0, x[this->counter]);
      GEOSCoordSeq_setY_r(this->context, seq, 0, y[this->counter]);

      geometry = GEOSGeom_createPoint_r(this->context, seq);
    }

    this->counter = this->counter + 1;
    return geometry;
  }

  size_t size() {
    return (this->x).size();
  }
};

class XYExporter: public GeometryExporter {
public:
  NumericVector x;
  NumericVector y;
  size_t counter;

  void init(GEOSContextHandle_t context, size_t size) {
    NumericVector x(size);
    NumericVector y(size);
    this->x = x;
    this->y = y;

    this->context = context;
    this->counter = 0;
  }

  void putNext(GEOSGeometry* geometry) {
    double x, y;
    if (geometry == NULL) {
      x = NA_REAL;
      y = NA_REAL;
    } else {
      if (GEOSGeomTypeId_r(this->context, geometry) != GEOSGeomTypes::GEOS_POINT) {
        stop("Can't represent a non-point as a geo_xy()");
      }

      if (GEOSGetSRID_r(context, geometry) != 0) {
        Function warning("warning");
        warning("Dropping SRID in cast to geo_xy()");
      }

      // geos doesn't differentiate between POINT (nan, nan) and POINT EMPTY
      if (GEOSisEmpty_r(this->context, geometry)) {
        x = NA_REAL;
        y = NA_REAL;
      } else {
        GEOSGeomGetX_r(this->context, geometry, &x);
        GEOSGeomGetY_r(this->context, geometry, &y);
      }
    }

    this->x[this->counter] = x;
    this->y[this->counter] = y;

    this->counter = this->counter + 1;
  }

  SEXP finish() {
    List result = List::create(
      _["x"] = this->x,
      _["y"] = this->y
    );
    result.attr("class") = CharacterVector::create("geovctrs_xy", "geovctr", "vctrs_rcrd", "vctrs_vctr");
    return result;
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
  size_t counter;

  SegmentProvider(List segment) {
    List start = segment["start"];
    List end = segment["end"];
    this->x0 = start["x"];
    this->y0 = start["y"];
    this->x1 = end["x"];
    this->y1 = end["y"];
    this->srid = segment["srid"];
  }

  void init(GEOSContextHandle_t context) {
    this->context = context;
    this->counter = 0;
  }

  GEOSGeometry* getNext() {
    double x0, y0, x1, y1;
    int srid;
    GEOSGeometry* geometry;

    x0 = this->x0[this->counter];
    y0 = this->y0[this->counter];
    x1 = this->x1[this->counter];
    y1 = this->y1[this->counter];
    srid = this->srid[this->counter];

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
      geometry = GEOSGeom_createEmptyLineString_r(this->context);
      GEOSSetSRID_r(this->context, geometry, srid);
    } else {
      GEOSCoordSequence* seq = GEOSCoordSeq_create_r(this->context, 2, 2);

      // start
      GEOSCoordSeq_setX_r(this->context, seq, 0, this->x0[this->counter]);
      GEOSCoordSeq_setY_r(this->context, seq, 0, this->y0[this->counter]);

      // end
      GEOSCoordSeq_setX_r(this->context, seq, 1, this->x1[this->counter]);
      GEOSCoordSeq_setY_r(this->context, seq, 1, this->y1[this->counter]);

      geometry = GEOSGeom_createLineString_r(context, seq);
      GEOSSetSRID_r(this->context, geometry, this->srid[this->counter]);
    }

    this->counter = this->counter + 1;
    return geometry;
  }

  size_t size() {
    return (this->x0).size();
  }
};

class SegmentExporter: public GeometryExporter {
public:
  NumericVector x0;
  NumericVector y0;
  NumericVector x1;
  NumericVector y1;
  IntegerVector srid;
  size_t counter;

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

    this->context = context;
    this->counter = 0;
  }

  void putNext(GEOSGeometry* geometry) {
    double x0, y0, x1, y1;
    int srid;

    if (geometry == NULL) {
      x0 = NA_REAL;
      y0 = NA_REAL;
      x1 = NA_REAL;
      y1 = NA_REAL;
      srid = NA_INTEGER;
    } else {
      if (GEOSGeomTypeId_r(this->context, geometry) != GEOSGeomTypes::GEOS_LINESTRING) {
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
        const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(this->context, geometry);
        GEOSCoordSeq_getX_r(this->context, seq, 0, &x0);
        GEOSCoordSeq_getY_r(this->context, seq, 0, &y0);
        GEOSCoordSeq_getX_r(this->context, seq, 1, &x1);
        GEOSCoordSeq_getY_r(this->context, seq, 1, &y1);
      }

      srid = GEOSGetSRID_r(this->context, geometry);
    }

    this->x0[this->counter] = x0;
    this->y0[this->counter] = y0;
    this->x1[this->counter] = x1;
    this->y1[this->counter] = y1;
    this->srid[this->counter] = srid;

    this->counter = this->counter + 1;
  }

  SEXP finish() {
    List p1 = List::create(
      _["x"] = this->x0,
      _["y"] = this->y0
    );
    p1.attr("class") = CharacterVector::create("geovctrs_xy", "geovctr", "vctrs_rcrd", "vctrs_vctr");

    List p2 = List::create(
      _["x"] = this->x1,
      _["y"] = this->y1
    );
    p2.attr("class") = CharacterVector::create("geovctrs_xy", "geovctr", "vctrs_rcrd", "vctrs_vctr");

    List result = List::create(_["start"] = p1, _["end"] = p2, _["srid"] = this->srid);
    result.attr("class") = CharacterVector::create("geovctrs_segment", "geovctr", "vctrs_rcrd", "vctrs_vctr");

    return result;
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
  size_t counter;

  GeoRectProvider(List rect) {
    this->xmin = rect["xmin"];
    this->ymin = rect["ymin"];
    this->xmax = rect["xmax"];
    this->ymax = rect["ymax"];
    this->srid = rect["srid"];
  }

  void init(GEOSContextHandle_t context) {
    this->context = context;
    this->counter = 0;
  }

  GEOSGeometry* getNext() {
    double xmin1, ymin1, xmax1, ymax1;
    int srid;
    GEOSGeometry* geometry;

    xmin1 = this->xmin[this->counter];
    ymin1 = this->ymin[this->counter];
    xmax1 = this->xmax[this->counter];
    ymax1 = this->ymax[this->counter];
    srid = this->srid[this->counter];

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
      geometry = GEOSGeom_createEmptyPolygon_r(this->context);
      GEOSSetSRID_r(this->context, geometry, srid);
    } else {
      // counter clockwise!
      GEOSCoordSequence* seq = GEOSCoordSeq_create_r(this->context, 5, 2);
      GEOSCoordSeq_setX_r(this->context, seq, 0, xmin1); GEOSCoordSeq_setY_r(this->context, seq, 0, ymin1);
      GEOSCoordSeq_setX_r(this->context, seq, 1, xmax1); GEOSCoordSeq_setY_r(this->context, seq, 1, ymin1);
      GEOSCoordSeq_setX_r(this->context, seq, 2, xmax1); GEOSCoordSeq_setY_r(this->context, seq, 2, ymax1);
      GEOSCoordSeq_setX_r(this->context, seq, 3, xmin1); GEOSCoordSeq_setY_r(this->context, seq, 3, ymax1);
      GEOSCoordSeq_setX_r(this->context, seq, 4, xmin1); GEOSCoordSeq_setY_r(this->context, seq, 4, ymin1);

      GEOSGeometry* shell = GEOSGeom_createLinearRing_r(context, seq);
      GEOSGeometry* holes[0];
      geometry = GEOSGeom_createPolygon_r(context, shell, holes, 0);
      GEOSSetSRID_r(this->context, geometry, srid);
    }

    this->counter = this->counter + 1;
    return geometry;
  }

  size_t size() {
    return (this->xmin).size();
  }
};

#endif
