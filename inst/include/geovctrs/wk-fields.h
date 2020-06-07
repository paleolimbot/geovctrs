
#ifndef GEOVCTRS_WK_FIELDS_H
#define GEOVCTRS_WK_FIELDS_H

#include "wk/io.h"
#include "wk/reader.h"
#include "wk/writer.h"

template<typename ContainerType>
class GeovctrsFieldsProvider: public WKProvider {
public:
  GeovctrsFieldsProvider(ContainerType container, size_t size):
    container(container), size(size), index(SIZE_T_MAX) {}

  template<typename ItemType, typename VectorType>
  ItemType field(size_t field) {
    VectorType vector = this->container[field];
    return vector[this->index];
  }

  virtual size_t nFields() {
    return this->container.size();
  }

  size_t nFeatures() {
    return this->size;
  }

  // whether or not a feature is null has to be resolved
  // by individual type readers...without knowing anything
  // about the vector types it isn't possible
  bool featureIsNull() {
    return false;
  }

  void reset() {
    this->index = SIZE_T_MAX;
  }

  bool seekNextFeature() {
    if (this->index == SIZE_T_MAX) {
      this->index = 0;
    } else {
      this->index++;
    }
    return this->index < this->nFeatures();
  }

protected:
  ContainerType container;

private:
  size_t size;
  size_t index;
};

template<typename ContainerType>
class GeovctrsFieldsExporter: public WKExporter {
public:
  GeovctrsFieldsExporter(ContainerType container, size_t size):
    WKExporter(size), index(0) {}

  template<typename ItemType, typename VectorType>
  void setField(size_t field, ItemType value) {
    VectorType vector = this->container[field];
    vector[this->index] = value;
  }

  size_t nFields() {
    return this->container.size();
  }

protected:
  ContainerType container;
  size_t index;
};

template<typename ContainerType>
class GeovctrsWKFieldsReader: public WKReader {
public:
  GeovctrsWKFieldsReader(GeovctrsFieldsProvider<ContainerType>& provider):
    WKReader(provider), provider(provider) {}

protected:
  GeovctrsFieldsProvider<ContainerType>& provider;
};


template<typename ContainerType>
class GeovctrsWKFieldsWriter: public WKWriter {
public:
  GeovctrsWKFieldsWriter(GeovctrsFieldsExporter<ContainerType>& exporter): exporter(exporter) {}

  virtual void nextNull(size_t featureId) = 0;

protected:
  GeovctrsFieldsExporter<ContainerType>& exporter;
};

#endif
