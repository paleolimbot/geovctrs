
#ifndef GEOVCTRS_OPERATOR_HPP
#define GEOVCTRS_OPERATOR_HPP

#include "geos-handler.hpp"
#include "provider.hpp"
#include "exporter.hpp"
#include <Rcpp.h>
using namespace Rcpp;

// ----- resolvers -----

class GeovctrsProviderFactory {
public:
  static std::unique_ptr<GeovctrsProvider> get(SEXP data) {
    std::unique_ptr<GeovctrsProvider> provider;

    if (Rf_inherits(data, "geovctrs_wkt")) {
      provider = std::unique_ptr<GeovctrsProvider> { new GeovctrsWKTProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_wkb")) {
      provider = std::unique_ptr<GeovctrsProvider> { new GeovctrsWKBProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_xy")) {
      provider = std::unique_ptr<GeovctrsProvider> { new GeovctrsXYProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_segment")) {
      provider = std::unique_ptr<GeovctrsProvider> { new GeovctrsSegmentProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_rect")) {
      provider = std::unique_ptr<GeovctrsProvider> { new GeovctrsRectProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_collection")) {
      provider = std::unique_ptr<GeovctrsProvider> { new GeovctrsCollectionProvider(data) };
    } else {
      stop("Can't resolve GeovctrsProvider");
    }

    if (provider->size() == 1) {
      return std::unique_ptr<GeovctrsProvider> { new GeovctrsConstantProvider(provider.release()) };
    } else {
      return provider;
    }
  }
};

class GeovctrsExporterFactory {
public:
  static std::unique_ptr<GeovctrsExporter> get(SEXP ptype) {
    if (Rf_inherits(ptype, "geovctrs_wkt")) {
      return std::unique_ptr<GeovctrsExporter> { new GeovctrsWKTExporter(ptype) };
    } else if(Rf_inherits(ptype, "geovctrs_wkb")) {
      return std::unique_ptr<GeovctrsExporter> { new GeovctrsWKBExporter(ptype) };
    } else if(Rf_inherits(ptype, "geovctrs_collection")) {
      return std::unique_ptr<GeovctrsExporter> { new GeovctrsCollectionExporter() };
    } else if(Rf_inherits(ptype, "geovctrs_xy")) {
      return std::unique_ptr<GeovctrsExporter> { new GeovctrsXYExporter() };
    } else if(Rf_inherits(ptype, "geovctrs_segment")) {
      return std::unique_ptr<GeovctrsExporter> { new GeovctrsSegmentExporter() };
    } else {
      stop("Can't resolve GeovctrsExporter");
    }
  }
};

// ------------ base class ------------

class GeovctrsBaseOperator {
public:

  GeovctrsBaseOperator() {
    this->provider = std::unique_ptr<GeovctrsProvider>(nullptr);
    this->geometry = NULL;
  }

  virtual void initProvider(SEXP data) {
    this->provider = GeovctrsProviderFactory::get(data);
  }

  virtual SEXP operate() {
    this->initOperator();
    this->init(this->handler.context, this->size());

    for (size_t i=0; i < this->size(); i++) {
      if ((i + 1) % 1000 == 0) {
        checkUserInterrupt();
      }

      this->loopNext(this->handler.context, i);

      if (this->geometry != NULL) {
        GEOSGeom_destroy_r(this->handler.context, this->geometry);
        this->geometry = NULL;
      }
    }

    return this->finishOperator();
  }

  virtual size_t size() {
    return this->commonSize;
  }

protected:
  // including this as a field ensures that it exists
  // for the entire lifetime of the object
  // manually instantiating it causes an extra copy to be
  // created which causes errors
  // ...because C++ is nuts
  GeovctrsGEOSHandler handler;
  size_t commonSize;
  std::unique_ptr<GeovctrsProvider> provider;
  GEOSGeometry* geometry = NULL;

  // these are the functions that may be overridden by individual
  // operator subclasses
  // loopNext() must be implemented

  virtual size_t maxParameterLength() {
    return 1;
  }

  virtual void init(GEOSContextHandle_t context, size_t size) {

  }

  virtual void loopNext(GEOSContextHandle_t context, size_t i) = 0;

  virtual SEXP assemble(GEOSContextHandle_t context) {
    return R_NilValue;
  }

  // subclasses must implement their own deleters if they need
  // to clean up memory...this deleter won't call
  // methods of subclasses
  // all deleters of the class heiarchy get called
  virtual ~GeovctrsBaseOperator() {
    if (this->provider) {
      this->provider->finish(this->handler.context);
    }

    if (this->geometry != NULL) {
      GEOSGeom_destroy_r(this->handler.context, this->geometry);
      this->geometry = NULL;
    }
  }

private:
  void initOperator() {
    if (!this->provider) {
      stop("GeovctrsBaseOperator.initProvider() was never called");
    }

    this->provider->init(this->handler.context);
    this->commonSize = GeovctrsBaseOperator::recycledSize(this->maxParameterLength(), this->provider->size());
  }

  SEXP finishOperator() {
    SEXP result = this->assemble(this->handler.context);
    return result;
  }

  static size_t recycledSize(IntegerVector sizes) {
    size_t commonSize;
    IntegerVector nonConstantSizes = sizes[sizes != 1];
    if (nonConstantSizes.size() == 0) {
      commonSize = 1;
    } else {
       commonSize = nonConstantSizes[0];
    }

    for (size_t i=0; i < nonConstantSizes.size(); i++) {
      if (nonConstantSizes[i] != commonSize) {
        stop("Providers/parameters with incompatible lengths passed to GeovctrsBaseOperator");
      }
    }

    return commonSize;
  }

  static size_t recycledSize(size_t size1, size_t size2) {
    return recycledSize(IntegerVector::create(size1, size2));
  }

  static size_t recycledSize(size_t size1, size_t size2, size_t size3) {
    return recycledSize(IntegerVector::create(size1, size2, size3));
  }
};

// ------------- unary operators ----------------

class GeovctrsOperator: public GeovctrsBaseOperator {
public:

  virtual void loopNext(GEOSContextHandle_t context, size_t i) {
    this->geometry = this->provider->getNext(context, i);
    this->operateNext(context, this->geometry, i);
  }

  virtual void operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) = 0;
};

class GeovctrsGeometryOperator: public GeovctrsBaseOperator {
public:
  std::unique_ptr<GeovctrsExporter> exporter;
  GEOSGeometry* result;

  GeovctrsGeometryOperator() {
    std::unique_ptr<GeovctrsExporter> exporter = std::unique_ptr<GeovctrsExporter>(nullptr);
    this->result = NULL;
  }

  virtual void initExporter(SEXP ptype) {
    this->exporter = GeovctrsExporterFactory::get(ptype);
  }

  void init(GEOSContextHandle_t context, size_t size) {
    if (!this->exporter) {
      stop("GeovctrsGeometryOperator.initExporter() was never called");
    }
    this->exporter->init(context, this->size());
  }

  void loopNext(GEOSContextHandle_t context, size_t i) {
    this->geometry = this->provider->getNext(context, i);

    if (this->geometry == NULL) {
      this->result = this->operateNextNULL(context, i);
    } else {
      this->result = this->operateNext(context, this->geometry, i);
    }

    this->exporter->putNext(context, this->result, i);

    // Most of the time, the geometry is
    // modified instead of copied, so GeovctrsBaseOperator::operate()
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

  ~GeovctrsGeometryOperator() {
    if (this->exporter) {
      this->exporter->finish(this->handler.context);
    }

    // only clean up if this->result is a copy
    if (this->result != NULL && (this->result != this->geometry)) {
      GEOSGeom_destroy_r(this->handler.context, this->result);
      this->result = NULL;
    }
  }

  virtual GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) = 0;
  virtual GEOSGeometry* operateNextNULL(GEOSContextHandle_t context, size_t i) {
    return NULL;
  }
};

template <class VectorType, class ScalarType>
class GeovctrsVectorOperator: public GeovctrsBaseOperator {
public:
  VectorType data;

  void init(GEOSContextHandle_t context, size_t size) {
    VectorType data(size);
    this->data = data;
  }

  void loopNext(GEOSContextHandle_t context, size_t i) {
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

  virtual ScalarType operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) = 0;
  virtual ScalarType operateNextNULL(GEOSContextHandle_t context, size_t i) {
    return VectorType::get_na();
  }
};

class GeovctrsRecursiveOperator: public GeovctrsBaseOperator {
public:
  size_t featureId;
  int partId;
  int ringId;
  unsigned int coordinateId;
  int recursionLevel;

  void loopNext(GEOSContextHandle_t context, size_t i) {
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

  virtual void nextFeature(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (geometry == NULL) {
      this->nextNULL(context, i);
      return;
    } else {
      this->nextGeometry(context, geometry);
    }
  }

  virtual void nextNULL(GEOSContextHandle_t context, size_t i) {

  }

  virtual void nextError(GEOSContextHandle_t context, const char* message, size_t i) {
    stop(message);
  }

  virtual void nextGeometry(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
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

  virtual void nextPoint(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->nextGeometryDefault(context, geometry);
  }

  virtual void nextLinestring(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    this->nextGeometryDefault(context, geometry);
  }

  virtual void nextPolygon(GEOSContextHandle_t context, const GEOSGeometry* geometry) {
    int nInteriorRings = GEOSGetNumInteriorRings_r(context, geometry);
    this->nextGeometryDefault(context, GEOSGetExteriorRing_r(context, geometry));
    for(int i=0; i < nInteriorRings; i++) {
      this->nextGeometry(context, GEOSGetInteriorRingN_r(context, geometry, i));
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
        this->nextCoordinate(context, xi, yi, zi);
      }
    } else {
      for (unsigned int i=0; i < size; i++) {
        this->coordinateId = i;
        GEOSCoordSeq_getX_r(context, seq, i, &xi);
        GEOSCoordSeq_getY_r(context, seq, i, &yi);
        this->nextCoordinate(context, xi, yi);
      }
    }
  }

  virtual void nextCoordinate(GEOSContextHandle_t context, double x, double y) {
    this->nextCoordinate(context, x, y, NA_REAL);
  }

  virtual void nextCoordinate(GEOSContextHandle_t context, double x, double y, double z) {

  }
};

# endif
