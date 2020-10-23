/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */


#include <map>
#include <string>
#include <ostream>

#include "Eigen/Dense"
#include "eckit/exception/Exceptions.h"

#include "DataContainer.h"


namespace Ingester
{
    void DataContainer::add(const std::string& fieldName, const std::shared_ptr<DataObject> data)
    {
        if (hasKey(fieldName))
        {
            std::ostringstream errorStr;
            errorStr << "ERROR: Field called " << fieldName << " already exists.";
            throw eckit::BadParameter(errorStr.str());
        }

        dataMap_.insert({fieldName, data});
    }

    std::shared_ptr<DataObject> DataContainer::get(const std::string& fieldName) const
    {
        if (!hasKey(fieldName))
        {
            std::ostringstream errorStr;
            errorStr << "ERROR: Field called " << fieldName << " doesn't exists.";
            throw eckit::BadParameter(errorStr.str());
        }

        return dataMap_.at(fieldName);
    }

    bool DataContainer::hasKey(const std::string& fieldName) const
    {
        bool hasKey = false;
        if (dataMap_.find(fieldName) != dataMap_.end())
        {
            hasKey = true;
        }

        return hasKey;
    }
}  // namespace Ingester

