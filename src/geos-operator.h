
#ifndef GEOS_OPERATOR_H
#define GEOS_OPERATOR_H

#include "geos-handler.h"
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

  Operator() {
    this->provider = std::unique_ptr<GeometryProvider>(nullptr);
  }

  virtual void initProvider(SEXP data) {
    this->provider = GeometryProviderFactory::get(data);
  }

  virtual SEXP operate() {
    this->initOperator();
    this->init(this->handler.context, this->size());

    for (size_t i=0; i < this->size(); i++) {
      if (i % 1000 == 0) {
        checkUserInterrupt();
      }

      this->loopNext(this->handler.context, i);
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
  RcppGEOSHandler handler;
  size_t commonSize;
  std::unique_ptr<GeometryProvider> provider;
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

  virtual ~Operator() {
    if (this->provider) {
      this->provider->finish(this->handler.context);
    }
  }

private:
  void initOperator() {
    if (!this->provider) {
      stop("Operator: initProvider() was never called");
    }

    this->provider->init(this->handler.context);
    this->commonSize = Operator::recycledSize(this->maxParameterLength(), this->provider->size());
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

class UnaryOperator: public Operator {
public:

  virtual void loopNext(GEOSContextHandle_t context, size_t i) {
    this->geometry = this->provider->getNext(context, i);
    this->operateNext(context, this->geometry, i);
  }

  virtual void operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) = 0;
};

class UnaryGeometryOperator: public Operator {
public:
  std::unique_ptr<GeometryExporter> exporter;
  GEOSGeometry* result;

  UnaryGeometryOperator() {
    std::unique_ptr<GeometryExporter> exporter = std::unique_ptr<GeometryExporter>(nullptr);
  }

  virtual void initExporter(SEXP ptype) {
    this->exporter = GeometryExporterFactory::get(ptype);
  }

  void init(GEOSContextHandle_t context, size_t size) {
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
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return this->exporter->assemble(context);
  }

  ~UnaryGeometryOperator() {
    if (this->exporter) {
      this->exporter->finish(this->handler.context);
    }
  }

  virtual GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) = 0;
  virtual GEOSGeometry* operateNextNULL(GEOSContextHandle_t context, size_t i) {
    return NULL;
  }
};

template <class VectorType, class ScalarType>
class UnaryVectorOperator: public Operator {
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

# endif
