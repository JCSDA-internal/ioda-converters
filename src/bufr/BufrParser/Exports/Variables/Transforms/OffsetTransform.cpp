/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "OffsetTransform.h"
#include "IngesterTypes.h"


namespace Ingester
{
    OffsetTransform::OffsetTransform(const double offset) :
      offset_(offset)
    {
    }

    void OffsetTransform::apply(std::shared_ptr<DataObjectBase>& dataObject)
    {
      if (auto object = std::dynamic_pointer_cast<DataObject<float>>(dataObject))
      {
        auto data = object->getRawData();
        for (auto& val : data)
        {
            if (val != MissingValue) val += offset_;
        }

        object->setRawData(data);
      }
    }

}  // namespace Ingester
