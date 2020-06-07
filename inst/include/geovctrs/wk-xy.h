
#ifndef GEOVCTRS_WK_XY_H
#define GEOVCTRS_WK_XY_H

#include <cmath>
#include "geovctrs/wk-fields.h"

template<typename ContainerType, typename RealVectorType>
class GeovctrsWKXYReader: public GeovctrsWKFieldsReader<ContainerType> {
public:
  GeovctrsWKXYReader(GeovctrsFieldsProvider<ContainerType>& provider):
    GeovctrsWKFieldsReader<ContainerType>(provider) {}

  void readFeature(size_t featureId) {
    this->handler->nextFeatureStart(featureId);

    double x = this->provider.template field<double, RealVectorType>(0);
    double y = this->provider.template field<double, RealVectorType>(1);

    WKGeometryMeta meta(WKGeometryType::Point, false, false, false);
    meta.hasSize = true;

    // treat NA, NA as an empty point
    if (std::isnan(x) && std::isnan(y)) {
      meta.size = 0;
      this->handler->nextGeometryStart(meta, WKReader::PART_ID_NONE);
      this->handler->nextGeometryEnd(meta, WKReader::PART_ID_NONE);
    } else {
      meta.size = 1;
      this->handler->nextGeometryStart(meta, WKReader::PART_ID_NONE);
      this->handler->nextCoordinate(meta, WKCoord::xy(x, y), 0);
      this->handler->nextGeometryEnd(meta, WKReader::PART_ID_NONE);
    }

    this->handler->nextFeatureEnd(featureId);
  }
};

#endif
