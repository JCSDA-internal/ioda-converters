/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once


#include <map>
#include <string>
#include <vector>
#include <memory>
#include "Eigen/Dense"

#include "DataObject/DataObject.h"
#include "IngesterTypes.h"


namespace Ingester
{
    class DataContainer
    {
     public:
        DataContainer() = default;

        void add(const std::string& fieldName, std::shared_ptr<DataObject> data);
        std::shared_ptr<DataObject> get(const std::string& fieldName) const;
        bool hasKey(const std::string& fieldName) const;

        inline unsigned int size() const { return size_; }
        inline void setSize(const unsigned int size) { size_ = size; }

     private:
        std::map<std::string, std::shared_ptr<DataObject>> dataMap_;
        unsigned int size_ = 0;
    };
}  // namespace Ingester


