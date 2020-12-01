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
    /// List of possible categories
    typedef std::vector<std::string> Categories;

    /// Map of data set id's to vector of possible value strings
    typedef std::map<std::string, Categories> CategoryMap;

    /// Map string paths (ex: variable/radiance) to DataObject
    typedef std::map<std::string, std::shared_ptr<DataObject>> DataSetMap;

    /// Map category combo (ex: SatId/sat_1, GeoBox/lat_25_30__lon_23_26) to the relevant DataSetMap
    typedef std::map<std::vector<std::string>, DataSetMap> DataSets;


    /// \brief Collection of DataObjects that a Parser collected identified by their exported name
    class DataContainer
    {
     public:
        DataContainer(const CategoryMap& categoryMap);

        /// \brief Add a DataObject to the collection
        /// \param fieldName The unique (export) string that identifies this data
        /// \param data The DataObject to store
        void add(const Categories& categoryId,
                 const std::string& fieldName,
                 std::shared_ptr<DataObject> data);

        /// \brief Get a DataObject from the collection
        /// \param fieldName The name of the data object ot get
        std::shared_ptr<DataObject> get(const Categories& categoryId,
                                        const std::string& fieldName) const;

        /// \brief Check if DataObject with name is available
        /// \param fieldName The name of the object
        bool hasKey(const Categories& categoryId,
                    const std::string& fieldName) const;

        // Getters
        inline size_t size(const Categories& categoryId) const
        {
            return  dataSets_.at(categoryId).begin()->second->nrows();
        }

        inline std::vector<Categories> getAllCategories()
        {
            std::vector<Categories> allCategories;

            for (const auto& dataSetPair : dataSets_)
            {
                allCategories.push_back(dataSetPair.first);
            }

            return allCategories;
        };

        inline std::vector<std::vector<size_t>> getCategoryIdxs() { return categoryIdxs_; }

        inline CategoryMap getCategoryMap() { return categoryMap_; }

     private:
        const CategoryMap categoryMap_;
        std::vector<std::vector<size_t>> categoryIdxs_;
        DataSets dataSets_;

        void makeDataSets();
    };
}  // namespace Ingester
