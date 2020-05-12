
#ifndef GEOVCTRS_GEOS_EXPORTER_HPP
#define GEOVCTRS_GEOS_EXPORTER_HPP

#include <geos_c.h>
#include "feature-factory.hpp"
#include "geovctrs/factory.hpp"
#include <memory.h>
#include <Rcpp.h>
using namespace Rcpp;

// WKB can't really represent an empty point, but this hack
// works with the WKB reader in GEOS and sf.
// other consumers of geo_wkb() objects might want to be careful
// passing this on to external software
const size_t WKB_HACK_EMPTY_POINT_SIZE = 21;
const unsigned char WKB_HACK_EMPTY_POINT_LITTLE_ENDIAN[] = {
  // little endian
  0x01,
  // geometry type: point 2D
  0x01, 0x00, 0x00, 0x00,
  // x coordinates (nan)
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f,
  // y coordinate (nan)
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f
};


class GeovctrsGEOSExporter {
public:
  virtual void init(GEOSContextHandle_t context, R_xlen_t size) {}
  virtual void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) = 0;
  virtual SEXP assemble(GEOSContextHandle_t context) {
    return R_NilValue;
  }
  virtual void finish(GEOSContextHandle_t context) {}
  virtual ~GeovctrsGEOSExporter() {}
};

class GeovctrsGEOSWKTExporter: public GeovctrsGEOSExporter {
public:
  CharacterVector data;
  bool trim;
  int precision;
  int dimensions;
  GEOSWKTWriter *wktWriter;

  GeovctrsGEOSWKTExporter(CharacterVector ptype) {
    this->trim = true;
    this->precision = 16;
    this->dimensions = 3;
    this->wktWriter = NULL;
  }

  void init(GEOSContextHandle_t context, R_xlen_t size) {
    this->wktWriter = GEOSWKTWriter_create_r(context);
    GEOSWKTWriter_setTrim_r(context, this->wktWriter, this->trim);
    GEOSWKTWriter_setRoundingPrecision_r(context, this->wktWriter, this->precision);
    GEOSWKTWriter_setOutputDimension_r(context, this->wktWriter, this->dimensions);

    CharacterVector data(size);
    this->data = data;
  }

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
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



class GeovctrsGEOSWKBExporter: public GeovctrsGEOSExporter {
public:
  List data;
  GEOSWKBWriter *wkbWriter;
  int includeSRID;
  int dimensions;
  int endian;

  GeovctrsGEOSWKBExporter(List ptype) {
    this->includeSRID = NA_INTEGER;
    this->dimensions = 3;
    this->endian = NA_INTEGER;
    this->wkbWriter = NULL;
  }

  void init(GEOSContextHandle_t context, R_xlen_t size) {
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

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
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
        size_t size = WKB_HACK_EMPTY_POINT_SIZE;
        RawVector raw(size);
        memcpy(&(raw[0]), WKB_HACK_EMPTY_POINT_LITTLE_ENDIAN, size);
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



class GeovctrsGEOSCollectionExporter: public GeovctrsGEOSExporter {
public:
  List data;
  IntegerVector srid;

  void init(GEOSContextHandle_t context, R_xlen_t size) {
    NumericVector data(size);
    IntegerVector srid(size);
    this->data = data;
    this->srid = srid;
  }

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    if (geometry == NULL) {
      this->data[i] = R_NilValue;
      this->srid[i] = NA_INTEGER;
    } else {
      this->data[i] = GeovctrsGEOSFeatureFactory::getFeature(context, geometry);
      this->srid[i] = GEOSGetSRID_r(context, geometry);
    }
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return GeovctrsFactory::newCollection(this->data, this->srid);
  }
};



class GeovctrsGEOSXYExporter: public GeovctrsGEOSExporter {
public:
  NumericVector x;
  NumericVector y;
  NumericVector z;
  bool hasZ;

  GeovctrsGEOSXYExporter(bool hasZ) {
    this->hasZ = hasZ;
  }

  void init(GEOSContextHandle_t context, R_xlen_t size) {
    this->x = NumericVector(size);
    this->y = NumericVector(size);
    this->z = NumericVector(size);
  }

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    double x, y, z;
    if (geometry == NULL) {
      x = NA_REAL;
      y = NA_REAL;
      z = NA_REAL;
    } else {
      if (GEOSGeomTypeId_r(context, geometry) != GEOSGeomTypes::GEOS_POINT) {
        stop("Can't represent a non-point as a geo_xy()");
      }

      if (GEOSGetSRID_r(context, geometry) != 0) {
        Function warning("warning");
        warning("Dropping SRID in cast to geo_xy()");
      }

      // geos doesn't differentiate between POINT (nan nan) and POINT EMPTY
      if (GEOSisEmpty_r(context, geometry)) {
        x = NA_REAL;
        y = NA_REAL;
        z = NA_REAL;
      } else if (GEOSHasZ_r(context, geometry)) {
        // GEOSGeomGetZ_r() doesn't exist in GEOS 3.5.0
        const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(context, geometry);
        GEOSCoordSeq_getX_r(context, seq, 0, &x);
        GEOSCoordSeq_getY_r(context, seq, 0, &y);
        GEOSCoordSeq_getZ_r(context, seq, 0, &z);
        this->hasZ = true;
      } else {
        GEOSGeomGetX_r(context, geometry, &x);
        GEOSGeomGetY_r(context, geometry, &y);
        z = NA_REAL;
      }
    }

    this->x[i] = x;
    this->y[i] = y;
    this->z[i] = z;
  }

  SEXP assemble(GEOSContextHandle_t context) {
    if (this->hasZ) {
      return GeovctrsFactory::newXYZ(this->x, this->y, this->z);
    } else {
      return GeovctrsFactory::newXY(this->x, this->y);
    }
  }
};



class GeovctrsGEOSSegmentExporter: public GeovctrsGEOSExporter {
public:
  NumericVector x0;
  NumericVector y0;
  NumericVector x1;
  NumericVector y1;
  IntegerVector srid;

  void init(GEOSContextHandle_t context, R_xlen_t size) {
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

  void putNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
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

      if (!GEOSisEmpty_r(context, geometry) && GEOSGeom_getCoordinateDimension_r(context, geometry) > 2) {
        stop("Can't represent 3D segments using a geo_segment()");
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

#endif
