
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

  virtual SEXP operate() = 0;

  virtual void init() {

  }

  virtual size_t maxParameterLength() {
    return 1;
  }

  virtual void finish() {

  }

  virtual size_t size() {
    return this->commonSize;
  }

  virtual void finishProvider() {

  }

  virtual ~Operator() {

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

private:
  void initBase() {
    this->context = geos_init();
    this->provider->init(this->context);

    IntegerVector allSizes = IntegerVector::create(
      this->maxParameterLength(),
      this->provider->size()
    );

    IntegerVector nonConstantSizes = allSizes[allSizes != 1];
    if (nonConstantSizes.size() == 0) {
      this->commonSize = 1;
    } else {
      this->commonSize = nonConstantSizes[0];
    }

    for (size_t i=0; i<nonConstantSizes.size(); i++) {
      if (nonConstantSizes[i] != this->commonSize) {
        stop("Providers with incompatible lengths passed to BinaryGeometryOperator");
      }
    }

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

    IntegerVector allSizes = IntegerVector::create(
      this->maxParameterLength(),
      this->provider->size()
    );

    IntegerVector nonConstantSizes = allSizes[allSizes != 1];
    if (nonConstantSizes.size() == 0) {
      this->commonSize = 1;
    } else {
      this->commonSize = nonConstantSizes[0];
    }

    for (size_t i=0; i<nonConstantSizes.size(); i++) {
      if (nonConstantSizes[i] != this->commonSize) {
        stop("Providers with incompatible lengths passed to BinaryGeometryOperator");
      }
    }
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

  virtual void initProvider(SEXP provider);
  virtual SEXP operate();
  virtual ScalarType operateNext(GEOSGeometry* geometry) = 0;
  virtual ScalarType operateNextNULL();
  virtual SEXP assemble();

private:
  void initBase();
  VectorType finishBase();
};

// ------ unary vector operators implementation --------
// I don't know  why these can't be defined in geos-operator.cpp
// but putting them there results in a linker error

template <class VectorType, class ScalarType>
void UnaryVectorOperator<VectorType, ScalarType>::initProvider(SEXP provider) {
  this->provider = GeometryProviderFactory::get(provider);
}

template <class VectorType, class ScalarType>
void UnaryVectorOperator<VectorType, ScalarType>::initBase() {
  this->context = geos_init();
  this->provider->init(this->context);

  IntegerVector allSizes = IntegerVector::create(
    this->maxParameterLength(),
    this->provider->size()
  );

  IntegerVector nonConstantSizes = allSizes[allSizes != 1];
  if (nonConstantSizes.size() == 0) {
    this->commonSize = 1;
  } else {
    this->commonSize = nonConstantSizes[0];
  }

  for (size_t i=0; i<nonConstantSizes.size(); i++) {
    if (nonConstantSizes[i] != this->commonSize) {
      stop("Providers with incompatible lengths passed to BinaryGeometryOperator");
    }
  }

  VectorType data(this->size());
  this->data = data;
}

template <class VectorType, class ScalarType>
SEXP UnaryVectorOperator<VectorType, ScalarType>::operate() {
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

template <class VectorType, class ScalarType>
ScalarType UnaryVectorOperator<VectorType, ScalarType>::operateNextNULL() {
  return VectorType::get_na();
}

template <class VectorType, class ScalarType>
SEXP UnaryVectorOperator<VectorType, ScalarType>::assemble() {
  return this->data;
}

template <class VectorType, class ScalarType>
VectorType UnaryVectorOperator<VectorType, ScalarType>::finishBase() {
  this->provider->finish(this->context);
  geos_finish(this->context);
  return this->assemble();
}

# endif
