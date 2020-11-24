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
#include "EncoderTypes.h"


namespace IodaEncoder
{
    /// \brief Collection of DataObjects that a Parser collected identified by their exported name
    class DataContainer
    {
     public:
        DataContainer() = default;

        /// \brief Add a DataObject to the collection
        /// \param fieldName The unique (export) string that identifies this data
        /// \param data The DataObject to store
        void add(const std::string& fieldName, std::shared_ptr<DataObject> data);

        /// \brief Get a DataObject from the collection
        /// \param fieldName The name of the data object ot get
        std::shared_ptr<DataObject> get(const std::string& fieldName) const;

        /// \brief Check if DataObject with name is available
        /// \param fieldName The name of the object
        bool hasKey(const std::string& fieldName) const;

        // Getters
        inline unsigned int size() const { return size_; }
        inline void setSize(const unsigned int size) { size_ = size; }

     private:
        /// \brief The data
        std::map<std::string, std::shared_ptr<DataObject>> dataMap_;

        /// \brief Number of rows of data stored in DataObjects
        unsigned int size_ = 0;
    };
}  // namespace IodaEncoder


