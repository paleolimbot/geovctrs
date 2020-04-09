
#ifndef GEOS_OPERATOR_H
#define GEOS_OPERATOR_H

#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

// ----- resolvers -----

class GeometryProviderFactory {
public:
  static std::unique_ptr<GeometryProvider> get(SEXP data) {
    std::unique_ptr<GeometryProvider> provider;

    if (Rf_inherits(data, "geovctrs_wkt")) {
      provider = std::unique_ptr<GeometryProvider> { new WKTGeometryProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_wkb")) {
      provider = std::unique_ptr<GeometryProvider> { new WKBGeometryProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_xy")) {
      provider = std::unique_ptr<GeometryProvider> { new XYProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_segment")) {
      provider = std::unique_ptr<GeometryProvider> { new SegmentProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_rect")) {
      provider = std::unique_ptr<GeometryProvider> { new GeoRectProvider(data) };
    } else if(Rf_inherits(data, "geovctrs_collection")) {
      provider = std::unique_ptr<GeometryProvider> { new GeoCollectionProvider(data) };
    } else {
      stop("Can't resolve GeometryProvider");
    }

    if (provider->size() == 1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(provider.release()) };
    } else {
      return provider;
    }
  }
};

class GeometryExporterFactory {
public:
  static std::unique_ptr<GeometryExporter> get(SEXP ptype) {
    if (Rf_inherits(ptype, "geovctrs_wkt")) {
      return std::unique_ptr<GeometryExporter> { new WKTGeometryExporter(ptype) };
    } else if(Rf_inherits(ptype, "geovctrs_wkb")) {
      return std::unique_ptr<GeometryExporter> { new WKBGeometryExporter(ptype) };
    } else if(Rf_inherits(ptype, "geovctrs_collection")) {
      return std::unique_ptr<GeometryExporter> { new GeoCollectionExporter() };
    } else if(Rf_inherits(ptype, "geovctrs_xy")) {
      return std::unique_ptr<GeometryExporter> { new XYExporter() };
    } else if(Rf_inherits(ptype, "geovctrs_segment")) {
      return std::unique_ptr<GeometryExporter> { new SegmentExporter() };
    } else {
      stop("Can't resolve GeometryExporter");
    }
  }
};

// ------------ base class ------------

class Operator {
public:
  size_t commonSize;
  size_t counter;
  GEOSContextHandle_t context;

  // this is the main interface: this and initProvider() are
  // the only methods that should get called from the outside
  virtual SEXP operate() = 0;

  // these are the functions that may be overridden by individual
  // operator subclasses
  virtual void init() {

  }

  virtual size_t maxParameterLength() {
    return 1;
  }

  virtual void finish() {

  }

  virtual void finishProvider() {

  }

  virtual ~Operator() {

  }

  // these shouldn't be overridden except in this file
  // to support the base operator types
  virtual void initBase() {

  }

  virtual SEXP finishBase() {
    return R_NilValue;
  }

  virtual size_t size() {
    return this->commonSize;
  }

  // static functions
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
        stop("Providers/parameters with incompatible lengths passed to Operator");
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

class UnaryGeometryOperator: public Operator {
public:
  std::unique_ptr<GeometryProvider> provider;
  std::unique_ptr<GeometryExporter> exporter;

  virtual void initProvider(SEXP provider, SEXP exporter) {
    this->provider = GeometryProviderFactory::get(provider);
    this->exporter = GeometryExporterFactory::get(exporter);
  }

  virtual SEXP operate() {
    this->initBase();
    this->init();

    // TODO: there is probably a memory leak here, but
    // GEOSGeom_destroy_r(this->context, geometry) gives
    // an error
    GEOSGeometry* geometry;
    GEOSGeometry* result;

    try {
      for (size_t i=0; i < this->size(); i++) {
        checkUserInterrupt();
        this->counter = i;
        geometry = this->provider->getNext(this->context, i);

        if (geometry == NULL) {
          result = this->operateNextNULL();
        } else {
          result = this->operateNext(geometry);
        }

        this->exporter->putNext(this->context, result, i);
      }
    } catch(Rcpp::exception e) {
      this->finish();
      throw e;
    }

    this->finish();
    return this->finishBase();
  }

  virtual GEOSGeometry* operateNext(GEOSGeometry* geometry) = 0;

  virtual GEOSGeometry* operateNextNULL() {
    return NULL;
  }

  void initBase() {
    this->context = geos_init();
    this->provider->init(this->context);
    this->commonSize = Operator::recycledSize(this->maxParameterLength(), this->provider->size());
    this->exporter->init(this->context, this->size());
  }

  SEXP finishBase() {
    this->provider->finish(this->context);
    SEXP value = this->exporter->finish(this->context);
    geos_finish(this->context);
    return value;
  }
};

// ----- unary vector operators -----

class UnaryOperator: public Operator {
public:
  std::unique_ptr<GeometryProvider> provider;

  virtual void initProvider(SEXP provider) {
    this->provider = GeometryProviderFactory::get(provider);
  }

  virtual SEXP operate() {
    this->initBase();
    this->init();

    // TODO: there is probably a memory leak here, but
    // GEOSGeom_destroy_r(this->context, geometry) gives
    // an error
    GEOSGeometry* geometry;

    try {
      for (size_t i=0; i < this->size(); i++) {
        checkUserInterrupt();
        this->counter = i;
        geometry = this->provider->getNext(this->context, i);
        this->operateNext(geometry);
      }
    } catch(Rcpp::exception e) {
      this->finish();
      throw e;
    } catch(std::exception e) {
      this->finish();
      throw e;
    }

    this->finish();
    return this->finishBase();
  }

  virtual void operateNext(GEOSGeometry* geometry) = 0;

  virtual SEXP assemble() {
    return R_NilValue;
  }

  void initBase() {
    this->context = geos_init();
    this->provider->init(this->context);
    this->commonSize = Operator::recycledSize(this->maxParameterLength(), this->provider->size());
  }

  SEXP finishBase() {
    this->provider->finish(this->context);
    geos_finish(this->context);
    return this->assemble();
  }
};

template <class VectorType, class ScalarType>
class UnaryVectorOperator: public Operator {
public:
  std::unique_ptr<GeometryProvider> provider;
  VectorType data;

  virtual void initProvider(SEXP provider) {
    this->provider = GeometryProviderFactory::get(provider);
  }

  virtual SEXP operate()  {
    this->initBase();
    this->init();

    // TODO: there is probably a memory leak here, but
    // GEOSGeom_destroy_r(this->context, geometry) gives
    // an error
    GEOSGeometry* geometry;
    ScalarType result;

    try {
      for (size_t i=0; i < this->size(); i++) {
        checkUserInterrupt();
        this->counter = i;
        geometry = this->provider->getNext(this->context, i);

        if (geometry == NULL) {
          result = this->operateNextNULL();
        } else {
          result = this->operateNext(geometry);
        }

        this->data[i] = result;
      }
    } catch(Rcpp::exception e) {
      this->finish();
      throw e;
    } catch(std::exception  e) {
      this->finish();
      throw e;
    }

    this->finish();
    return this->finishBase();
  }

  virtual ScalarType operateNext(GEOSGeometry* geometry) = 0;

  virtual ScalarType operateNextNULL() {
    return VectorType::get_na();
  }

  virtual SEXP assemble() {
    return this->data;
  }

  void initBase() {
    this->context = geos_init();
    this->provider->init(this->context);
    this->commonSize = Operator::recycledSize(this->maxParameterLength(), this->provider->size());

    VectorType data(this->size());
    this->data = data;
  }

  SEXP finishBase() {
    this->provider->finish(this->context);
    geos_finish(this->context);
    return this->assemble();
  }
};

# endif
