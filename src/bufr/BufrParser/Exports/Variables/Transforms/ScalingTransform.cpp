/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "ScalingTransform.h"
#include "IngesterTypes.h"
#include "BufrParser/Query/Constants.h"

namespace Ingester
{
    ScalingTransform::ScalingTransform(const double scaling) :
      scaling_(scaling)
    {
    }

    void ScalingTransform::apply(std::shared_ptr<DataObjectBase>& dataObject)
    {
        dataObject->multiplyBy(scaling_);
    }
}  // namespace Ingester
