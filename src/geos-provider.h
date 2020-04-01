
#ifndef GEOS_PROVIDER_H
#define GEOS_PROVIDER_H

#include "geos-base.h"
#include <memory.h>
#include <Rcpp.h>
using namespace Rcpp;

// --- base

class GeometryProvider {
public:
  GEOSContextHandle_t context;
  size_t recycleSize;

  virtual void init(GEOSContextHandle_t context);
  virtual GEOSGeometry* getNext() = 0;
  virtual void finish();
  virtual size_t size() = 0;

  virtual ~GeometryProvider();
};

class GeometryExporter {
public:
  GEOSContextHandle_t context;

  virtual void init(GEOSContextHandle_t context, size_t size);
  virtual void putNext(GEOSGeometry* geometry) = 0;
  virtual SEXP finish();

  virtual ~GeometryExporter();
};

class ConstantGeometryProvider: public GeometryProvider {
public:
  std::unique_ptr<GeometryProvider> baseProvider;
  GEOSGeometry* geometry;

  ConstantGeometryProvider(GeometryProvider* baseProvider);
  void init(GEOSContextHandle_t context);
  GEOSGeometry* getNext();
  void finish();
  size_t size();
};

using namespace Rcpp;

// --- WKT

class WKTGeometryProvider: public GeometryProvider {
public:
  CharacterVector data;
  GEOSWKTReader *wkt_reader;
  size_t counter;

  WKTGeometryProvider(CharacterVector data);
  void init(GEOSContextHandle_t context);
  GEOSGeometry* getNext();
  void finish();
  size_t size();
};

class WKTGeometryExporter: public GeometryExporter {
public:
  CharacterVector data;
  GEOSWKTWriter *wkt_writer;
  size_t counter;

  WKTGeometryExporter();
  void init(GEOSContextHandle_t context, size_t size);
  void putNext(GEOSGeometry* geometry);
  SEXP finish();
};

// --- WKB

class WKBGeometryProvider: public GeometryProvider {
public:
  List data;
  GEOSWKBReader *wkb_reader;
  size_t counter;

  WKBGeometryProvider(List data);
  void init(GEOSContextHandle_t context);
  GEOSGeometry* getNext();
  void finish();
  size_t size();
};

class WKBGeometryExporter: public GeometryExporter {
public:
  List data;
  GEOSWKBWriter *wkb_writer;
  size_t counter;

  WKBGeometryExporter();
  void init(GEOSContextHandle_t context, size_t size);
  void putNext(GEOSGeometry* geometry);
  SEXP finish();
};

// --- GeoCollection

class GeoCollectionProvider: public GeometryProvider {
public:
  List features;
  IntegerVector srid;
  size_t counter;

  GeoCollectionProvider(List data);
  void init(GEOSContextHandle_t context);
  GEOSGeometry* getNext();
  size_t size();
};

class GeoCollectionExporter: public GeometryExporter {
public:
  List data;
  IntegerVector srid;
  size_t counter;

  GeoCollectionExporter();
  void init(GEOSContextHandle_t context, size_t size);
  void putNext(GEOSGeometry* geometry);
  SEXP finish();
};

// --- XY

class XYProvider: public GeometryProvider {
public:
  NumericVector x;
  NumericVector y;
  size_t counter;

  XYProvider(NumericVector x, NumericVector y);
  void init(GEOSContextHandle_t context);
  GEOSGeometry* getNext();
  size_t size();
};

class XYExporter: public GeometryExporter {
public:
  NumericVector x;
  NumericVector y;
  size_t counter;

  void init(GEOSContextHandle_t context, size_t size);
  void putNext(GEOSGeometry* geometry);
  SEXP finish();
};

// --- segment ----

class SegmentProvider: public GeometryProvider {
public:
  NumericVector x0;
  NumericVector y0;
  NumericVector x1;
  NumericVector y1;
  size_t counter;

  SegmentProvider(NumericVector x0, NumericVector y0, NumericVector x1, NumericVector y1);
  void init(GEOSContextHandle_t context);
  GEOSGeometry* getNext();
  size_t size();
};

class SegmentExporter: public GeometryExporter {
public:
  NumericVector x0;
  NumericVector y0;
  NumericVector x1;
  NumericVector y1;
  size_t counter;

  void init(GEOSContextHandle_t context, size_t size);
  void putNext(GEOSGeometry* geometry);
  SEXP finish();
};

// --- rect ----

class GeoRectProvider: public GeometryProvider {
public:
  NumericVector xmin;
  NumericVector ymin;
  NumericVector xmax;
  NumericVector ymax;
  size_t counter;

  GeoRectProvider(NumericVector xmin, NumericVector ymin, NumericVector xmax, NumericVector ymax);
  void init(GEOSContextHandle_t context);
  GEOSGeometry* getNext();
  size_t size();
};

class GeoRectExporter: public GeometryExporter {
public:
  NumericVector xmin;
  NumericVector ymin;
  NumericVector xmax;
  NumericVector ymax;
  size_t counter;

  void init(GEOSContextHandle_t context, size_t size);
  void putNext(GEOSGeometry* geometry);
  SEXP finish();
};

// --- geometry provider/exporter resolvers

std::unique_ptr<GeometryProvider> resolve_provider(SEXP data);
std::unique_ptr<GeometryExporter> resolve_exporter(SEXP ptype);

#endif
