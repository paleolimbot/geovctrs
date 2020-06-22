
#ifndef GEOVCTRS_WK_XYZ_H
#define GEOVCTRS_WK_XYZ_H

#include <cmath>
#include "geovctrs/wk-fields.hpp"

template<typename ContainerType, typename RealVectorType>
class GeovctrsWKXYZReader: public GeovctrsWKFieldsReader<ContainerType> {
public:
  GeovctrsWKXYZReader(GeovctrsFieldsProvider<ContainerType>& provider):
    GeovctrsWKFieldsReader<ContainerType>(provider) {}

  void readFeature(size_t featureId) {
    this->handler->nextFeatureStart(featureId);

    double x = this->provider.template field<double, RealVectorType>(0);
    double y = this->provider.template field<double, RealVectorType>(1);
    double z = this->provider.template field<double, RealVectorType>(2);

    WKGeometryMeta meta(WKGeometryType::Point, true, false, false);
    meta.hasSize = true;

    // treat NA, NA, NA as an empty point
    if (std::isnan(x) && std::isnan(y) && std::isnan(z)) {
      meta.size = 0;
      this->handler->nextGeometryStart(meta, WKReader::PART_ID_NONE);
      this->handler->nextGeometryEnd(meta, WKReader::PART_ID_NONE);
    } else {
      meta.size = 1;
      this->handler->nextGeometryStart(meta, WKReader::PART_ID_NONE);
      this->handler->nextCoordinate(meta, WKCoord::xyz(x, y, z), 0);
      this->handler->nextGeometryEnd(meta, WKReader::PART_ID_NONE);
    }

    this->handler->nextFeatureEnd(featureId);
  }
};

template<typename ContainerType, typename RealVectorType>
class GeovctrsWKXYZWriter: public GeovctrsWKFieldsWriter<ContainerType> {
public:
  GeovctrsWKXYZWriter(GeovctrsFieldsExporter<ContainerType>& exporter):
    GeovctrsWKFieldsWriter<ContainerType>(exporter) {}

  virtual void nextFeatureStart(size_t featureId) {
    GeovctrsWKFieldsWriter<ContainerType>::nextFeatureStart(featureId);
  }

  void nextNull(size_t featureId) {
    this->exporter.template setField<double, RealVectorType>(0, NAN);
    this->exporter.template setField<double, RealVectorType>(1, NAN);
    this->exporter.template setField<double, RealVectorType>(2, NAN);
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    if (meta.geometryType != WKGeometryType::Point) {
      throw std::runtime_error("Can't create XY from a non-point");
    }

    if (meta.size == 0) {
      this->exporter.template setField<double, RealVectorType>(0, NAN);
      this->exporter.template setField<double, RealVectorType>(1, NAN);
      this->exporter.template setField<double, RealVectorType>(2, NAN);
    }
  }

  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->exporter.template setField<double, RealVectorType>(0, coord.x);
    this->exporter.template setField<double, RealVectorType>(1, coord.y);
    if (coord.hasZ) {
      this->exporter.template setField<double, RealVectorType>(2, coord.z);
    } else {
      this->exporter.template setField<double, RealVectorType>(2, NAN);
    }
  }

};

#endif
