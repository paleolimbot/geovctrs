
#include "geos-provider.h"
#include "geos-coords.h"
#include "geos-coords-write.h"
#include <Rcpp.h>
using namespace Rcpp;

// ---------- geometry provider implementations -------------

// --- base provider

void GeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
}


void GeometryProvider::finish() {

}

GeometryProvider::~GeometryProvider() {

}

// --- constant provider

ConstantGeometryProvider::ConstantGeometryProvider(GeometryProvider* baseProvider) {
  this->baseProvider = std::unique_ptr<GeometryProvider> { baseProvider };
  this->geometry = nullptr;
}

void ConstantGeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->baseProvider->init(context);
}

GEOSGeometry* ConstantGeometryProvider::getNext() {
  if (this->geometry == nullptr) {
    this->geometry = this->baseProvider->getNext();
  }
  return this->geometry;
}

void ConstantGeometryProvider::finish() {
  this->baseProvider->finish();
}

size_t ConstantGeometryProvider::size() {
  return 1;
}

// --- base exporter

void GeometryExporter::init(GEOSContextHandle_t context, size_t size) {
  this->context = context;
}

SEXP GeometryExporter::finish() {
  return R_NilValue;
}

GeometryExporter::~GeometryExporter() {

}

// --- WKT provider

WKTGeometryProvider::WKTGeometryProvider(CharacterVector data) {
  this->data = data;
  this->counter = 0;
}

void WKTGeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->wkt_reader = GEOSWKTReader_create_r(context);
}

GEOSGeometry* WKTGeometryProvider::getNext() {
  GEOSGeometry* geometry = GEOSWKTReader_read_r(
    this->context,
    this->wkt_reader,
    this->data[this->counter]
  );
  this->counter = this->counter + 1;
  return geometry;
}

void WKTGeometryProvider::finish() {
  GEOSWKTReader_destroy_r(this->context, this->wkt_reader);
}

size_t WKTGeometryProvider::size() {
  return (this->data).size();
}

// --- WKT exporter

WKTGeometryExporter::WKTGeometryExporter() {
  this->counter = 0;
}

void WKTGeometryExporter::init(GEOSContextHandle_t context, size_t size) {
  this->context = context;
  this->wkt_writer = GEOSWKTWriter_create_r(context);
  CharacterVector data(size);
  data.attr("class") = CharacterVector::create("geo_wkt", "geo", "vctrs_vctr");
  this->data = data;
}

void WKTGeometryExporter::putNext(GEOSGeometry* geometry) {
  std::string wkt_single;
  wkt_single = GEOSWKTWriter_write_r(this->context, wkt_writer, geometry);
  this->data[this->counter] = wkt_single;
  this->counter = this->counter + 1;
}

SEXP WKTGeometryExporter::finish() {
  GEOSWKTWriter_destroy_r(this->context, this->wkt_writer);
  return this->data;
}

// --- WKB provider

WKBGeometryProvider::WKBGeometryProvider(List data) {
  this->data = data;
  this->counter = 0;
}

void WKBGeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->wkb_reader = GEOSWKBReader_create_r(context);
}

GEOSGeometry* WKBGeometryProvider::getNext() {
  RawVector r = this->data[this->counter];
  GEOSGeometry* geometry = GEOSWKBReader_read_r(context, this->wkb_reader, &(r[0]), r.size());
  this->counter = this->counter + 1;
  return geometry;
}

void WKBGeometryProvider::finish() {
  GEOSWKBReader_destroy_r(this->context, this->wkb_reader);
}

size_t WKBGeometryProvider::size() {
  return (this->data).size();
}

// --- WKB exporter

WKBGeometryExporter::WKBGeometryExporter() {
  this->counter = 0;
  // -1 = guess, 0 = false, 1 = true
  this->useEWKB = -1;
}

void WKBGeometryExporter::init(GEOSContextHandle_t context, size_t size) {
  this->context = context;
  this->wkb_writer = GEOSWKBWriter_create_r(context);
  if (this->useEWKB == 1) {
    GEOSWKBWriter_setIncludeSRID_r(this->context, this->wkb_writer, 1);
  }

  List data(size);
  data.attr("class") = CharacterVector::create("geo_wkb", "geo", "vctrs_list_of", "vctrs_vctr");
  data.attr("ptype") = RawVector::create();
  this->data = data;
}

void WKBGeometryExporter::putNext(GEOSGeometry* geometry) {
  if (this->useEWKB == -1) {
    int sridFirst = GEOSGetSRID_r(this->context, geometry);
    if (sridFirst != 0) {
      GEOSWKBWriter_setIncludeSRID_r(this->context, this->wkb_writer, 1);
      this->useEWKB = 1;
    }  else {
      this->useEWKB = 0;
    }
  }

  size_t size;
  unsigned char *buf = GEOSWKBWriter_write_r(this->context, this->wkb_writer, geometry, &size);
  RawVector raw(size);
  memcpy(&(raw[0]), buf, size);
  GEOSFree_r(this->context, buf);

  this->data[this->counter] = raw;
  this->counter = this->counter + 1;
}

SEXP WKBGeometryExporter::finish() {
  GEOSWKBWriter_destroy_r(this->context, this->wkb_writer);
  return data;
}

// --- geo_collection provider

GeoCollectionProvider::GeoCollectionProvider(List data) {
  this->features = data["feature"];
  this->srid = data["srid"];
}

void GeoCollectionProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->counter = 0;
}

GEOSGeometry* GeoCollectionProvider::getNext() {
  GEOSGeometry* geometry = feature_from_geo_coord(this->context, this->features[this->counter]);
  GEOSSetSRID_r(context, geometry, this->srid[this->counter]);

  this->counter = this->counter + 1;
  return geometry;
}

size_t GeoCollectionProvider::size() {
  return (this->features).size();
}

// --- geo_collection() exporter

GeoCollectionExporter::GeoCollectionExporter() {

}

void GeoCollectionExporter::init(GEOSContextHandle_t context, size_t size) {
  IntegerVector srid(size);
  this->srid = srid;
  List data(size);
  this->data = data;
  this->context = context;
  this->counter = 0;
}

void GeoCollectionExporter::putNext(GEOSGeometry* geometry) {
  this->data[this->counter] = geometry_to_geo_coord(this->context, geometry);
  this->srid[this->counter] = GEOSGetSRID_r(this->context, geometry);

  this->counter = this->counter + 1;
}

SEXP GeoCollectionExporter::finish() {
  List out = List::create(_["feature"] = this->data, _["srid"] = this->srid);
  out.attr("class") = CharacterVector::create("geo_collection", "vctrs_rcrd", "vctrs_vctr");
  return out;
}

// --- XY provider

XYProvider::XYProvider(NumericVector x, NumericVector y) {
  this->x = x;
  this->y = y;
}

void XYProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->counter = 0;
}

GEOSGeometry* XYProvider::getNext() {
  GEOSGeometry* geometry;

  GEOSCoordSequence* seq = GEOSCoordSeq_create_r(this->context, 1, 2);
  GEOSCoordSeq_setX_r(this->context, seq, 0, x[this->counter]);
  GEOSCoordSeq_setY_r(this->context, seq, 0, y[this->counter]);

  geometry = GEOSGeom_createPoint_r(this->context, seq);

  this->counter = this->counter + 1;
  return geometry;
}

size_t XYProvider::size() {
  return (this->x).size();
}

// --- XY exporter -----

void XYExporter::init(GEOSContextHandle_t context, size_t size) {
  NumericVector x(size);
  NumericVector y(size);
  this->x = x;
  this->y = y;

  this->context = context;
  this->counter = 0;
}

void XYExporter::putNext(GEOSGeometry* geometry) {
  if (GEOSGeomTypeId_r(this->context, geometry) != GEOSGeomTypes::GEOS_POINT) {
    stop("Can't represent a non-point as a geo_xy()");
  }

  if (GEOSGetSRID_r(context, geometry) != 0) {
    Function warning("warning");
    warning("Dropping SRID in cast to geo_xy()");
  }

  // geos doesn't differentiate between POINT (nan, nan) and POINT EMPTY
  double x, y;
  if (GEOSisEmpty_r(this->context, geometry)) {
    x = NA_REAL;
    y = NA_REAL;
  } else {
    GEOSGeomGetX_r(this->context, geometry, &x);
    GEOSGeomGetY_r(this->context, geometry, &y);
  }

  this->x[this->counter] = x;
  this->y[this->counter] = y;

  this->counter = this->counter + 1;
}

SEXP XYExporter::finish() {
  List result = List::create(
    _["x"] = this->x,
    _["y"] = this->y
  );
  result.attr("class") = CharacterVector::create("geo_xy", "vctrs_rcrd", "vctrs_vctr");
  return result;
}

// ---- Segment provider

SegmentProvider::SegmentProvider(NumericVector x0, NumericVector y0,
                                 NumericVector x1, NumericVector y1) {
  this->x0 = x0;
  this->y0 = y0;
  this->x1 = x1;
  this->y1 = y1;
}

void SegmentProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->counter = 0;
}

GEOSGeometry* SegmentProvider::getNext() {
  GEOSCoordSequence* seq = GEOSCoordSeq_create_r(this->context, 2, 2);

  // start
  GEOSCoordSeq_setX_r(this->context, seq, 0, this->x0[this->counter]);
  GEOSCoordSeq_setY_r(this->context, seq, 0, this->y0[this->counter]);

  // end
  GEOSCoordSeq_setX_r(this->context, seq, 1, this->x1[this->counter]);
  GEOSCoordSeq_setY_r(this->context, seq, 1, this->y1[this->counter]);

  GEOSGeometry* geometry = GEOSGeom_createLineString_r(context, seq);

  this->counter = this->counter + 1;
  return geometry;
}

size_t SegmentProvider::size() {
  return (this->x0).size();
}

// ---- Segment exporter

void SegmentExporter::init(GEOSContextHandle_t context, size_t size) {
  NumericVector x0(size);
  NumericVector y0(size);
  NumericVector x1(size);
  NumericVector y1(size);
  this->x0 = x0;
  this->y0 = y0;
  this->x1 = x1;
  this->y1 = y1;

  this->context = context;
  this->counter = 0;
}

void SegmentExporter::putNext(GEOSGeometry* geometry) {
  if (GEOSGeomTypeId_r(this->context, geometry) != GEOSGeomTypes::GEOS_LINESTRING) {
    stop("Can't represent a non-linestring as a geo_segment()");
  }

  if (!GEOSisEmpty_r(context, geometry) && GEOSGeomGetNumPoints_r(context, geometry) != 2) {
    stop("linestrings must have exactly two points to be represented as a geo_segment()");
  }

  double x0, y0, x1, y1;

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

  this->x0[this->counter] = x0;
  this->y0[this->counter] = y0;
  this->x1[this->counter] = x1;
  this->y1[this->counter] = y1;

  this->counter = this->counter + 1;
}

SEXP SegmentExporter::finish() {
  List p1 = List::create(
    _["x"] = this->x0,
    _["y"] = this->y0
  );
  p1.attr("class") = CharacterVector::create("geo_xy", "vctrs_rcrd", "vctrs_vctr");

  List p2 = List::create(
    _["x"] = this->x1,
    _["y"] = this->y1
  );
  p2.attr("class") = CharacterVector::create("geo_xy", "vctrs_rcrd", "vctrs_vctr");

  List result = List::create(_["start"] = p1, _["end"] = p2);
  result.attr("class") = CharacterVector::create("geo_segment", "vctrs_rcrd", "vctrs_vctr");

  return result;
}

// ---- GeoRect provider

GeoRectProvider::GeoRectProvider(NumericVector xmin, NumericVector ymin,
                                 NumericVector xmax, NumericVector ymax, IntegerVector srid) {
  this->xmin = xmin;
  this->ymin = ymin;
  this->xmax = xmax;
  this->ymax = ymax;
  this->srid = srid;
}

void GeoRectProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->counter = 0;
}

GEOSGeometry* GeoRectProvider::getNext() {
  double xmin1, ymin1, xmax1, ymax1;
  xmin1 = this->xmin[this->counter];
  ymin1 = this->ymin[this->counter];
  xmax1 = this->xmax[this->counter];
  ymax1 = this->ymax[this->counter];

  // counter clockwise!
  GEOSCoordSequence* seq = GEOSCoordSeq_create_r(this->context, 5, 2);
  GEOSCoordSeq_setX_r(this->context, seq, 0, xmin1); GEOSCoordSeq_setY_r(this->context, seq, 0, ymin1);
  GEOSCoordSeq_setX_r(this->context, seq, 1, xmax1); GEOSCoordSeq_setY_r(this->context, seq, 1, ymin1);
  GEOSCoordSeq_setX_r(this->context, seq, 2, xmax1); GEOSCoordSeq_setY_r(this->context, seq, 2, ymax1);
  GEOSCoordSeq_setX_r(this->context, seq, 3, xmin1); GEOSCoordSeq_setY_r(this->context, seq, 3, ymax1);
  GEOSCoordSeq_setX_r(this->context, seq, 4, xmin1); GEOSCoordSeq_setY_r(this->context, seq, 4, ymin1);

  GEOSGeometry* shell = GEOSGeom_createLinearRing_r(context, seq);
  GEOSGeometry* holes[0];
  GEOSGeometry* geometry = GEOSGeom_createPolygon_r(context, shell, holes, 0);

  int srid = this->srid[this->counter];
  if (IntegerVector::is_na(srid)) {
    GEOSSetSRID_r(this->context, geometry, 0);
  } else {
    GEOSSetSRID_r(this->context, geometry, srid);
  }

  this->counter = this->counter + 1;
  return geometry;
}

size_t GeoRectProvider::size() {
  return (this->xmin).size();
}

// --- GeoRect exporter

void GeoRectExporter::init(GEOSContextHandle_t context, size_t size) {
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

  this->context = context;
  this->counter = 0;
}

void GeoRectExporter::putNext(GEOSGeometry* geometry) {
  double xmin1, ymin1, xmax1, ymax1;

  if (GEOSisEmpty_r(this->context, geometry)) {
    xmin1 = R_PosInf;
    ymin1 = R_PosInf;
    xmax1 = R_NegInf;
    ymax1 = R_NegInf;
  } else {

#ifdef HAVE361
  // these functions are not available in GEOS 3.5.x
  // which is the version on the (common) Xenial build image
  GEOSGeom_getXMin_r(this->context, geometry, &xmin1);
  GEOSGeom_getYMin_r(this->context, geometry, &ymin1);
  GEOSGeom_getXMax_r(this->context, geometry, &xmax1);
  GEOSGeom_getYMax_r(this->context, geometry, &ymax1);
#else
  List coords = geometry_to_geo_coord(context, geometry);
  List xy = coords["xy"];
  NumericVector x = as<NumericVector>(xy["x"]);
  NumericVector y = as<NumericVector>(xy["y"]);
  xmin1 = min(x);
  ymin1 = min(y);
  xmax1 = max(x);
  ymax1 = max(y);
#endif
  }

  this->xmin[this->counter] = xmin1;
  this->ymin[this->counter] = ymin1;
  this->xmax[this->counter] = xmax1;
  this->ymax[this->counter] = ymax1;
  this->srid[this->counter] = GEOSGetSRID_r(this->context, geometry);

  this->counter = this->counter + 1;
}

SEXP GeoRectExporter::finish() {
  List result = List::create(
    _["xmin"] = this->xmin,
    _["ymin"] = this->ymin,
    _["xmax"] = this->xmax,
    _["ymax"] = this->ymax,
    _["srid"] = this->srid
  );
  result.attr("class") = CharacterVector::create("geo_rect", "vctrs_rcrd", "vctrs_vctr");
  return result;
}

// ---------- geometry provider resolvers -------------

std::unique_ptr<GeometryProvider> resolve_provider(SEXP data) {
  if (Rf_inherits(data, "geo_wkt")) {
    CharacterVector dataChar = (CharacterVector) data;

    if (dataChar.size() ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new WKTGeometryProvider(dataChar)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new WKTGeometryProvider(dataChar) };
    }

  } else if(Rf_inherits(data, "geo_wkb")) {
    List dataList = (List) data;

    if (dataList.size() ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new WKBGeometryProvider(dataList)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new WKBGeometryProvider(dataList) };
    }

  } else if(Rf_inherits(data, "geo_xy")) {
    List xy = (List) data;
    NumericVector x = xy["x"];
    NumericVector y = xy["y"];

    if (x.size() ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new XYProvider(x, y)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new XYProvider(x, y) };
    }
  } else if(Rf_inherits(data, "geo_segment")) {
    List segment = (List) data;
    List start = segment["start"];
    List end = segment["end"];
    NumericVector x0 = start["x"];
    NumericVector y0 = start["y"];
    NumericVector x1 = end["x"];
    NumericVector y1 = end["y"];

    if (x0.size() ==  1) {
      return std::unique_ptr<GeometryProvider> {
        new ConstantGeometryProvider(new SegmentProvider(x0, y0, x1, y1))
      };
    } else {
      return std::unique_ptr<GeometryProvider> {
        new SegmentProvider(x0, y0, x1, y1)
      };
    }
  } else if(Rf_inherits(data, "geo_rect")) {
    List rect = (List) data;
    NumericVector xmin = rect["xmin"];
    NumericVector ymin = rect["ymin"];
    NumericVector xmax = rect["xmax"];
    NumericVector ymax = rect["ymax"];
    IntegerVector srid = rect["srid"];

    if (xmin.size() ==  1) {
      return std::unique_ptr<GeometryProvider> {
        new ConstantGeometryProvider(new GeoRectProvider(xmin, ymin, xmax, ymax, srid))
      };
    } else {
      return std::unique_ptr<GeometryProvider> {
        new GeoRectProvider(xmin, ymin, xmax, ymax, srid)
      };
    }
  } else if(Rf_inherits(data, "geo_collection")) {
    List col = (List) data;
    List features = col["feature"];

    if (features.size() ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new GeoCollectionProvider(col)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new GeoCollectionProvider(col) };
    }
  }

  stop("Can't resolve GeometryProvider");
}

std::unique_ptr<GeometryExporter> resolve_exporter(SEXP ptype) {
  if (Rf_inherits(ptype, "geo_wkt")) {
    return std::unique_ptr<GeometryExporter> { new WKTGeometryExporter() };

  } else if(Rf_inherits(ptype, "geo_wkb")) {
    return std::unique_ptr<GeometryExporter> { new WKBGeometryExporter() };

  } else if(Rf_inherits(ptype, "geo_rect")) {
    return std::unique_ptr<GeometryExporter> { new GeoRectExporter() };

  } else if(Rf_inherits(ptype, "geo_collection")) {
    return std::unique_ptr<GeometryExporter> { new GeoCollectionExporter() };
  } else if(Rf_inherits(ptype, "geo_xy")) {
    return std::unique_ptr<GeometryExporter> { new XYExporter() };
  } else if(Rf_inherits(ptype, "geo_segment")) {
    return std::unique_ptr<GeometryExporter> { new SegmentExporter() };
  }

  stop("Can't resolve GeometryExporter");
}
