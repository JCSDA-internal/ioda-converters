/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "DataObject.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"


namespace Ingester
{
    bool DataObjectBase::hasSamePath(const std::shared_ptr<DataObjectBase>& dataObject)
    {
        // Can't be the same
        if (dimPaths_.size() != dataObject->dimPaths_.size())
        {
            return false;
        }

        bool isSame = true;
        for (size_t pathIdx = 0; pathIdx < dimPaths_.size(); ++pathIdx)
        {
            if (dimPaths_[pathIdx] !=  dataObject->dimPaths_[pathIdx])
            {
                isSame = false;
                break;
            }
        }

        return isSame;
    }
}  // namespace Ingester
