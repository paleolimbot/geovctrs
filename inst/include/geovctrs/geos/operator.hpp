
#ifndef GEOVCTRS_GEOS_OPERATOR_HPP
#define GEOVCTRS_GEOS_OPERATOR_HPP

#include "handler.hpp"
#include "provider.hpp"
#include "exporter.hpp"
#include <Rcpp.h>
using namespace Rcpp;

// ----- resolvers -----

class GeovctrsGEOSProviderFactory {
public:
  static std::unique_ptr<GeovctrsGEOSProvider> get(SEXP data) {
    std::unique_ptr<GeovctrsGEOSProvider> provider;

    if (Rf_inherits(data, "wk_wkt")) {
      provider = std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSWKTProvider(data) };
    } else if (Rf_inherits(data, "wk_wkt")) {
      provider = std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSWKTProvider(data) };
    } else if(Rf_inherits(data, "wk_wkb")) {
      provider = std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSWKBProvider(data) };
    } else if(Rf_inherits(data, "wk_wkb")) {
      provider = std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSWKBProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_xyz")) {
      provider = std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSXYZProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_xy")) {
      provider = std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSXYProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_segment")) {
      provider = std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSSegmentProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_rect")) {
      provider = std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSRectProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_collection")) {
      provider = std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSCollectionProvider(data) };
    } else {
      stop("Can't resolve GeovctrsGEOSProvider");
    }

    if (provider->size() == 1) {
      return std::unique_ptr<GeovctrsGEOSProvider> { new GeovctrsGEOSConstantProvider(provider.release()) };
    } else {
      return provider;
    }
  }
};

class GeovctrsGEOSExporterFactory {
public:
  static std::unique_ptr<GeovctrsGEOSExporter> get(SEXP ptype) {
    if (Rf_inherits(ptype, "wk_wkt")) {
      return std::unique_ptr<GeovctrsGEOSExporter> { new GeovctrsGEOSWKTExporter(ptype) };
    } else if (Rf_inherits(ptype, "wk_wkt")) {
      return std::unique_ptr<GeovctrsGEOSExporter> { new GeovctrsGEOSWKTExporter(ptype) };
    } else if(Rf_inherits(ptype, "wk_wkb")) {
      return std::unique_ptr<GeovctrsGEOSExporter> { new GeovctrsGEOSWKBExporter(ptype) };
    } else if(Rf_inherits(ptype, "wk_wkb")) {
      return std::unique_ptr<GeovctrsGEOSExporter> { new GeovctrsGEOSWKBExporter(ptype) };
    } if(Rf_inherits(ptype, "geovctrs_collection")) {
      return std::unique_ptr<GeovctrsGEOSExporter> { new GeovctrsGEOSCollectionExporter() };
    } else if(Rf_inherits(ptype, "geovctrs_xyz")) {
      return std::unique_ptr<GeovctrsGEOSExporter> { new GeovctrsGEOSXYExporter(true) };
    } else if(Rf_inherits(ptype, "geovctrs_xy")) {
      return std::unique_ptr<GeovctrsGEOSExporter> { new GeovctrsGEOSXYExporter(false) };
    } else if(Rf_inherits(ptype, "geovctrs_segment")) {
      return std::unique_ptr<GeovctrsGEOSExporter> { new GeovctrsGEOSSegmentExporter() };
    } else {
      stop("Can't resolve GeovctrsGEOSExporter");
    }
  }
};

// ------------ base class ------------

class GeovctrsGEOSBaseOperator {
public:

  GeovctrsGEOSBaseOperator() {
    this->provider = std::unique_ptr<GeovctrsGEOSProvider>(nullptr);
    this->geometry = NULL;
  }

  virtual void initProvider(SEXP data) {
    this->provider = GeovctrsGEOSProviderFactory::get(data);
  }

  virtual SEXP operate() {
    this->initOperator();
    this->init(this->handler.context, this->size());

    for (R_xlen_t i=0; i < this->size(); i++) {
      if ((i + 1) % 10 == 0) {
        checkUserInterrupt();
        this->publishProgress(i);
      }

      this->loopNext(this->handler.context, i);
      this->provider->destroyNext(this->handler.context, this->geometry);
      this->geometry = NULL;
    }

    return this->finishOperator();
  }

  virtual R_xlen_t size() {
    return this->commonSize;
  }

protected:
  // including this as a field ensures that it exists
  // for the entire lifetime of the object
  // manually instantiating it causes an extra copy to be
  // created which causes errors
  // ...because C++ is nuts
  GeovctrsGEOSHandler handler;
  R_xlen_t commonSize;
  std::unique_ptr<GeovctrsGEOSProvider> provider;
  GEOSGeometry* geometry = NULL;

  // these are the functions that may be overridden by individual
  // operator subclasses
  // loopNext() must be implemented

  virtual R_xlen_t maxParameterLength() {
    return 1;
  }

  virtual void init(GEOSContextHandle_t context, R_xlen_t size) {

  }

  virtual void loopNext(GEOSContextHandle_t context, R_xlen_t i) = 0;

  virtual SEXP assemble(GEOSContextHandle_t context) {
    return R_NilValue;
  }

  virtual void publishProgress(R_xlen_t i) {

  }

  // subclasses must implement their own deleters if they need
  // to clean up memory...this deleter won't call
  // methods of subclasses
  // all deleters of the class heiarchy get called
  virtual ~GeovctrsGEOSBaseOperator() {
    if (this->provider) {
      this->provider->destroyNext(this->handler.context, this->geometry);
      this->provider->finish(this->handler.context);
    }
  }

private:

  void initOperator() {
    if (!this->provider) {
      stop("GeovctrsGEOSBaseOperator.initProvider() was never called");
    }

    this->provider->init(this->handler.context);
    this->commonSize = GeovctrsGEOSBaseOperator::recycledSize(
      this->maxParameterLength(),
      this->provider->size()
    );
  }

  SEXP finishOperator() {
    SEXP result = this->assemble(this->handler.context);
    return result;
  }

  static R_xlen_t recycledSize(R_xlen_t size1, R_xlen_t size2) {
    if (size1 == 1) {
      return size2;
    } else if (size2 == 1) {
      return size1;
    } else if (size1 == size2) {
      return size1;
    } else {
      stop("Incompatible lengths in GeovctrsOperator");
    }
  }

  static R_xlen_t recycledSize(R_xlen_t size1, R_xlen_t size2, R_xlen_t size3) {
    return recycledSize(recycledSize(size1, size2), size3);
  }
};

// ------------- unary operators ----------------

class GeovctrsGEOSOperator: public GeovctrsGEOSBaseOperator {
public:

  virtual void loopNext(GEOSContextHandle_t context, R_xlen_t i) {
    this->geometry = this->provider->getNext(context, i);
    this->operateNext(context, this->geometry, i);
  }

  virtual void operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) = 0;
};

class GeovctrsGEOSGeometryOperator: virtual public GeovctrsGEOSBaseOperator {
public:
  std::unique_ptr<GeovctrsGEOSExporter> exporter;
  GEOSGeometry* result;

  GeovctrsGEOSGeometryOperator() {
    std::unique_ptr<GeovctrsGEOSExporter> exporter = std::unique_ptr<GeovctrsGEOSExporter>(nullptr);
    this->result = NULL;
  }

  virtual void initExporter(SEXP ptype) {
    this->exporter = GeovctrsGEOSExporterFactory::get(ptype);
  }

  void init(GEOSContextHandle_t context, R_xlen_t size) {
    if (!this->exporter) {
      stop("GeovctrsGEOSGeometryOperator.initExporter() was never called");
    }
    this->exporter->init(context, this->size());
  }

  void loopNext(GEOSContextHandle_t context, R_xlen_t i) {
    this->geometry = this->provider->getNext(context, i);

    if (this->geometry == NULL) {
      this->result = this->operateNextNULL(context, i);
    } else {
      this->result = this->operateNext(context, this->geometry, i);
    }

    this->exporter->putNext(context, this->result, i);
    this->cleanNextGeometry(context);
  }

  void cleanNextGeometry(GEOSContextHandle_t context) {
    // Most of the time, the geometry is
    // modified instead of copied, so GeovctrsGEOSBaseOperator::operate()
    // will call GEOSGeom_destroy_r() (and any attempt to do so here
    // will segfault). Using `!=` to detect the case where copying
    // occurred.
    if (this->result != NULL && this->result != this->geometry) {
      GEOSGeom_destroy_r(context, this->result);
      this->result = NULL;
    } else {
      this->result = NULL;
    }
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return this->exporter->assemble(context);
  }

  ~GeovctrsGEOSGeometryOperator() {
    if (this->exporter) {
      this->exporter->finish(this->handler.context);
    }

    // only clean up if this->result is a copy
    if (this->result != NULL && (this->result != this->geometry)) {
      GEOSGeom_destroy_r(this->handler.context, this->result);
      this->result = NULL;
    }
  }

  virtual GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) = 0;
  virtual GEOSGeometry* operateNextNULL(GEOSContextHandle_t context, R_xlen_t i) {
    return NULL;
  }
};

template <class VectorType, class ScalarType>
class GeovctrsGEOSVectorOperator: public GeovctrsGEOSBaseOperator {
public:
  VectorType data;

  void init(GEOSContextHandle_t context, R_xlen_t size) {
    VectorType data(size);
    this->data = data;
  }

  void loopNext(GEOSContextHandle_t context, R_xlen_t i) {
    this->geometry = this->provider->getNext(context, i);
    ScalarType result;

    if (this->geometry == NULL) {
      result = this->operateNextNULL(context, i);
    } else {
      result = this->operateNext(context, this->geometry, i);
    }

    this->data[i] = result;
  }

  SEXP assemble(GEOSContextHandle_t) {
    return this->data;
  }

  virtual ScalarType operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) = 0;
  virtual ScalarType operateNextNULL(GEOSContextHandle_t context, R_xlen_t i) {
    return VectorType::get_na();
  }
};

class GeovctrsGEOSRecursiveOperator: virtual public GeovctrsGEOSBaseOperator {
public:
  R_xlen_t featureId;
  int partId;
  int ringId;
  unsigned int coordinateId;
  int recursionLevel;

  void loopNext(GEOSContextHandle_t context, R_xlen_t i) {
    this->featureId = i;
    this->partId = 0;
    this->ringId = 0;
    this->recursionLevel = 0;

    try {
      // assign geometry so that Operator destroys it
      this->geometry = this->provider->getNext(context, i);
      this->nextFeature(context, geometry, i);
    } catch(Rcpp::exception e) {
      this->nextError(context, e.what(), i);
    } catch(std::exception e) {
      provider->finish(context);
      throw e;
    }
  }

  virtual void nextFeature(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    if (geometry == NULL) {
      this->nextNULL(context, i);
      return;
    } else {
      this->nextGeometry(context, geometry);
    }
  }

  virtual void nextNULL(GEOSContextHandle_t context, R_xlen_t i) {

  }

  virtual void nextError(GEOSContextHandle_t context, const char* message, R_xlen_t i) {
    stop(message);
  }

  virtual void nextGeometry(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    // there is some inconsistency around the handling of an empty point
    // through the GEOS ages. This approach is intended to work for all
    // GEOS versions.
    if (GEOSisEmpty_r(context, geometry)) {
      this->nextEmpty(context, geometry);
      return;
    }

    switch(GEOSGeomTypeId_r(context, geometry)) {
    case GEOSGeomTypes::GEOS_POINT:
      this->nextPoint(context, geometry);
      break;
    case GEOSGeomTypes::GEOS_LINESTRING:
      this->nextLinestring(context, geometry);
      break;
    case GEOSGeomTypes::GEOS_LINEARRING:
      this->nextLinearring(context, geometry);
      break;
    case GEOSGeomTypes::GEOS_POLYGON:
      this->nextPolygon(context, geometry);
      break;
    case GEOSGeomTypes::GEOS_MULTIPOINT:
      this->nextMultipoint(context, geometry);
      break;
    case GEOSGeomTypes::GEOS_MULTILINESTRING:
      this->nextMultilinestring(context, geometry);
      break;
    case GEOSGeomTypes::GEOS_MULTIPOLYGON:
      this->nextMultipolygon(context, geometry);
      break;
    case GEOSGeomTypes::GEOS_GEOMETRYCOLLECTION:
      this->nextGeometrycollection(context, geometry);
      break;
    }
  }

  virtual void nextEmpty(GEOSContextHandle_t context, const GEOSGeometry* geometry) {

  }

  virtual void nextPoint(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->nextGeometryDefault(context, geometry);
  }

  virtual void nextLinestring(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->nextGeometryDefault(context, geometry);
  }

  virtual void nextPolygon(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    int nInteriorRings = GEOSGetNumInteriorRings_r(context, geometry);
    this->nextLinearring(context, GEOSGetExteriorRing_r(context, geometry));
    for(int i=0; i < nInteriorRings; i++) {
      this->nextLinearring(context, GEOSGetInteriorRingN_r(context, geometry, i));
    }
  }

  virtual void nextLinearring(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->ringId++;
    this->nextGeometryDefault(context, geometry);
  }

  virtual void nextMultipoint(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->nextMultiGeometryDefault(context, geometry);
  }

  virtual void nextMultilinestring(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->nextMultiGeometryDefault(context, geometry);
  }

  virtual void nextMultipolygon(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->nextMultiGeometryDefault(context, geometry);
  }

  virtual void nextGeometrycollection(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->recursionLevel++;
    this->nextMultiGeometryDefault(context, geometry);
    this->recursionLevel--;
  }

  virtual void nextGeometryDefault(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->nextCoordinateSequence(context, geometry, GEOSGeom_getCoordSeq_r(context, geometry));
  }

  virtual void nextMultiGeometryDefault(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    int nParts = GEOSGetNumGeometries_r(context, geometry);
    for (int i=0; i < nParts; i++) {
      this->partId = i;
      const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
      this->nextGeometry(context, part);
    }
  }

  virtual void nextCoordinateSequence(GEOSContextHandle_t context,
                                      const GEOSGeometry* geometry,
                                      const GEOSCoordSequence* seq) {
    unsigned int size;
    GEOSCoordSeq_getSize_r(context, seq, &size);
    int ndim = GEOSGeom_getCoordinateDimension_r(context, geometry);

    double xi;
    double yi;

    if (ndim == 3) {
      double zi;

      for (unsigned int i=0; i < size; i++) {
        this->coordinateId = i;
        GEOSCoordSeq_getX_r(context, seq, i, &xi);
        GEOSCoordSeq_getY_r(context, seq, i, &yi);
        GEOSCoordSeq_getZ_r(context, seq, i, &zi);
        this->nextXYZ(context, xi, yi, zi);
      }
    } else {
      for (unsigned int i=0; i < size; i++) {
        this->coordinateId = i;
        GEOSCoordSeq_getX_r(context, seq, i, &xi);
        GEOSCoordSeq_getY_r(context, seq, i, &yi);
        this->nextXY(context, xi, yi);
      }
    }
  }

  virtual void nextXY(GEOSContextHandle_t context, double x, double y) {
    this->nextXYZ(context, x, y, NA_REAL);
  }

  virtual void nextXYZ(GEOSContextHandle_t context, double x, double y, double z) {

  }
};

class GeovctrsGEOSRecursiveGeometryOperator: public GeovctrsGEOSGeometryOperator {
public:
  R_xlen_t featureId;
  int partId;
  int ringId;
  unsigned int coordinateId;
  int recursionLevel;

  void loopNext(GEOSContextHandle_t context, R_xlen_t i) {
    this->featureId = i;
    this->partId = 0;
    this->ringId = 0;
    this->recursionLevel = 0;

    try {
      // assign geometry so that Operator destroys it
      this->geometry = this->provider->getNext(context, i);
      this->result = this->nextFeature(context, geometry, i);
    } catch(Rcpp::exception e) {
      this->result = this->nextError(context, e.what(), i);
    } catch(std::exception e) {
      provider->finish(context);
      throw e;
    }

    this->exporter->putNext(context, this->result, i);
    this->cleanNextGeometry(context);
  }

  virtual GEOSGeometry* nextFeature(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    if (geometry == NULL) {
      return this->nextNULL(context, i);
    } else {
      return this->nextGeometry(context, geometry);
    }
  }

  virtual GEOSGeometry* nextNULL(GEOSContextHandle_t context, R_xlen_t i) {
    return NULL;
  }

  virtual GEOSGeometry* nextError(GEOSContextHandle_t context, const char* message, R_xlen_t i) {
    stop(message);
  }

  virtual GEOSGeometry* nextGeometry(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    // there is some inconsistency around the handling of an empty point
    // through the GEOS ages. This approach is intended to work for all
    // GEOS versions.
    if (GEOSisEmpty_r(context, geometry)) {
      return this->nextEmpty(context, geometry);
    }

    switch(GEOSGeomTypeId_r(context, geometry)) {
    case GEOSGeomTypes::GEOS_POINT:
      return this->nextPoint(context, geometry);
    case GEOSGeomTypes::GEOS_LINESTRING:
      return this->nextLinestring(context, geometry);
    case GEOSGeomTypes::GEOS_LINEARRING:
      return this->nextLinearring(context, geometry);
    case GEOSGeomTypes::GEOS_POLYGON:
      return this->nextPolygon(context, geometry);
    case GEOSGeomTypes::GEOS_MULTIPOINT:
      return this->nextMultipoint(context, geometry);
    case GEOSGeomTypes::GEOS_MULTILINESTRING:
      return this->nextMultilinestring(context, geometry);
    case GEOSGeomTypes::GEOS_MULTIPOLYGON:
      return this->nextMultipolygon(context, geometry);
    case GEOSGeomTypes::GEOS_GEOMETRYCOLLECTION:
      return this->nextGeometrycollection(context, geometry);
    }

    stop("Unrecognized geometry type");
  }

  virtual GEOSGeometry* nextEmpty(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    return GEOSGeom_clone_r(context, geometry);
  }

  virtual GEOSGeometry* nextPoint(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    GEOSCoordSequence* seq = this->nextGeometryDefault(context, geometry);

    // creating points with an empty coord sequence is a recent
    // GEOS feature...this bit makes things work for GEOS >=3.5 (at least)
    // note also that getting the coord sequence of an empty point in GEOS 3.8
    // will give you POINT (0 0) back!!! (hence the nextEmpty() approach)
    unsigned int newSize;
    GEOSCoordSeq_getSize_r(context, seq, &newSize);
    if (newSize == 0) {
      GEOSCoordSeq_destroy_r(context, seq);
      return GEOSGeom_createEmptyPoint_r(context);
    } else {
      return GEOSGeom_createPoint_r(context, seq);
    }
  }

  virtual GEOSGeometry* nextLinestring(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    GEOSCoordSequence* seq = this->nextGeometryDefault(context, geometry);
    return  GEOSGeom_createLineString_r(context, seq);
  }

  virtual GEOSGeometry* nextPolygon(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    GEOSGeometry* newShell = this->nextLinearring(context, GEOSGetExteriorRing_r(context, geometry));

    int nInteriorRings = GEOSGetNumInteriorRings_r(context, geometry);
    GEOSGeometry** newHoles = new GEOSGeometry*[nInteriorRings];
    try {
      for(int i=0; i < nInteriorRings; i++) {
        newHoles[i] = this->nextLinearring(context, GEOSGetInteriorRingN_r(context, geometry, i));
      }

      GEOSGeometry* out = GEOSGeom_createPolygon_r(context, newShell, newHoles, nInteriorRings);
      delete[] newHoles;
      return out;
    } catch (std::exception& e) {
      delete[] newHoles;
      throw e;
    }
  }

  virtual GEOSGeometry* nextLinearring(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->ringId++;
    GEOSCoordSequence* seq = this->nextGeometryDefault(context, geometry);
    return GEOSGeom_createLinearRing_r(context, seq);
  }

  virtual GEOSGeometry* nextMultipoint(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    return this->nextMultiGeometryDefault(context, geometry);
  }

  virtual GEOSGeometry* nextMultilinestring(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    return this->nextMultiGeometryDefault(context, geometry);
  }

  virtual GEOSGeometry* nextMultipolygon(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    return this->nextMultiGeometryDefault(context, geometry);
  }

  virtual GEOSGeometry* nextGeometrycollection(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->recursionLevel++;
    GEOSGeometry* result = this->nextMultiGeometryDefault(context, geometry);
    this->recursionLevel--;
    return result;
  }

  virtual GEOSGeometry* nextMultiGeometryDefault(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    int nParts = GEOSGetNumGeometries_r(context, geometry);

    GEOSGeometry** newParts = new GEOSGeometry*[nParts];
    try {
      for (int i=0; i < nParts; i++) {
        this->partId = i;
        newParts[i] = this->nextGeometry(context, GEOSGetGeometryN_r(context, geometry, i));
      }

      GEOSGeometry* out = GEOSGeom_createCollection_r(
        context,
        GEOSGeomTypeId_r(context, geometry),
        newParts,
        nParts
      );
      delete[] newParts;
      return out;
    } catch (std::exception& e) {
      delete[] newParts;
      throw e;
    }
  }

  virtual GEOSCoordSequence* nextGeometryDefault(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    const GEOSCoordSequence* originalSeq = GEOSGeom_getCoordSeq_r(context, geometry);
    return this->nextCoordinateSequence(context, geometry, originalSeq);
  }

  virtual GEOSCoordSequence* nextCoordinateSequence(GEOSContextHandle_t context,
                                                    const GEOSGeometry* geometry,
                                                    const GEOSCoordSequence* seq) {
    // need the geometry here because while GEOS doesn't do M
    // it may eventually, and this would be with the geom, not the
    // coord seq
    unsigned int size;
    GEOSCoordSeq_getSize_r(context, seq, &size);
    GEOSCoordSequence* newSeq = GEOSCoordSeq_clone_r(context, seq);
    int ndim = GEOSGeom_getCoordinateDimension_r(context, geometry);

    double xi, yi, zi;

    if (ndim == 3) {
      for (unsigned int i=0; i < size; i++) {
        this->coordinateId = i;
        GEOSCoordSeq_getX_r(context, newSeq, i, &xi);
        GEOSCoordSeq_getY_r(context, newSeq, i, &yi);
        GEOSCoordSeq_getZ_r(context, newSeq, i, &zi);

        this->nextXYZ(context, &xi, &yi, &zi);

        GEOSCoordSeq_setX_r(context, newSeq, i, xi);
        GEOSCoordSeq_setY_r(context, newSeq, i, yi);
        GEOSCoordSeq_setZ_r(context, newSeq, i, zi);
      }
    } else {
      for (unsigned int i=0; i < size; i++) {
        this->coordinateId = i;
        GEOSCoordSeq_getX_r(context, newSeq, i, &xi);
        GEOSCoordSeq_getY_r(context, newSeq, i, &yi);

        this->nextXY(context, &xi, &yi);

        GEOSCoordSeq_setX_r(context, newSeq, i, xi);
        GEOSCoordSeq_setY_r(context, newSeq, i, yi);
      }
    }

    return newSeq;
  }

  virtual void nextXY(GEOSContextHandle_t context, double* x, double* y) {

  }

  virtual void nextXYZ(GEOSContextHandle_t context, double* x, double* y, double* z) {
    this->nextXY(context, x, y);
  }

  // not used here, so make sure they aren't overridden
  GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    stop("GeovctrsGEOSRecursiveGeometryOperator::operateNext() is not relevant.");
  }
  GEOSGeometry* operateNextNULL(GEOSContextHandle_t context, R_xlen_t i) {
    stop("GeovctrsGEOSRecursiveGeometryOperator::operateNextNULL() is not relevant.");
  }
};

# endif
