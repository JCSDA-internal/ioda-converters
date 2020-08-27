/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "IngesterData.h"

#include <iostream>
#include <memory>


namespace Ingester
{
    void IngesterData::add(const std::string &fieldName, const IngesterArray &data)
    {
        if (dataMap_.find(fieldName) != dataMap_.end())
        {
            std::cout << "WARNING: Adding duplicate field called " << fieldName << std::endl;
        }

        dataMap_.insert({fieldName, data});
    }

    IngesterArray IngesterData::get(const std::string &fieldName)
    {
        if (dataMap_.find(fieldName) == dataMap_.end())
        {
            std::cout << "ERROR: Field called " << fieldName << " doesn't exist." << std::endl;
            abort();
        }

        return dataMap_[fieldName];
    }

    size_t IngesterData::size() const
    {
        size_t size = 0;
        if (dataMap_.size() > 0)
        {
            size = dataMap_.begin()->second.size();
        }

        return size;
    }
}  // namespace Ingester
