/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */


#include <map>
#include <string>
#include "Eigen/Dense"

#include "DataContainer.h"


namespace Ingester
{
    void DataContainer::add(const std::string& fieldName, const std::shared_ptr<DataObject> data)
    {
        if (hasKey(fieldName))
        {
            std::cout << "ERROR: Field called " << fieldName << " already exists." << std::endl;
            abort();
        }

        dataMap_.insert({fieldName, data});
    }

    std::shared_ptr<DataObject> DataContainer::get(const std::string& fieldName) const
    {
        if (!hasKey(fieldName))
        {
            std::cout << "ERROR: Field called " << fieldName << " doesn't exist." << std::endl;
            abort();
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

