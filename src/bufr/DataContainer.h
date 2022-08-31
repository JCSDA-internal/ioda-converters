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

#include "DataObject.h"
#include "IngesterTypes.h"


namespace Ingester
{
    /// List of possible category strings (for splitting data)
    typedef std::vector<std::string> SubCategory;

    /// Map of data set id's to vector of possible value strings
    typedef std::map<std::string, SubCategory> CategoryMap;

    /// Map string paths (ex: variable/radiance) to DataObject
    typedef std::map<std::string, std::shared_ptr<DataObjectBase>> DataSetMap;

    /// Map category combo (ex: SatId/sat_1, GeoBox/lat_25_30__lon_23_26) to the relevant DataSetMap
    typedef std::map<std::vector<std::string>, DataSetMap> DataSets;


    /// \brief Collection of DataObjects that a Parser collected identified by their exported name
    class DataContainer
    {
     public:
        /// \brief Simple constructor
        DataContainer();

        /// \brief Construct to create container with subcategories.
        /// \details constructor that creates a underlying data structure to store data in separate
        ///          sub categories defined by combining all possible combinations of categories
        ///          defined in the category map.
        /// \param categoryMap map of major category types ex: "SatId" to the possible sub types
        ///        for the category type ex: {"GOES-15", "GOES-16", "GOES-17"}.
        explicit DataContainer(const CategoryMap& categoryMap);

        /// \brief Add a DataObject to the collection
        /// \param fieldName The unique (export) string that identifies this data
        /// \param data The DataObject to store
        /// \param categoryId The vector<string> for the subcategory
        void add(const std::string& fieldName,
                 std::shared_ptr<DataObjectBase> data,
                 const SubCategory& categoryId = {});

        /// \brief Get a DataObject from the collection
        /// \param fieldName The name of the data object ot get
        /// \param categoryId The vector<string> for the subcategory
        std::shared_ptr<DataObjectBase> get(const std::string& fieldName,
                                            const SubCategory& categoryId = {}) const;

        /// \brief Get a DataObject for the group by field
        /// \param fieldName The name of the data object ot get
        /// \param categoryId The vector<string> for the subcategory
        std::shared_ptr<DataObjectBase> getGroupByObject(const std::string& fieldName,
                                                         const SubCategory& categoryId = {}) const;

        /// \brief Check if DataObject with name is available
        /// \param fieldName The name of the object
        /// \param categoryId The vector<string> for the subcategory
        bool hasKey(const std::string& fieldName,
                    const SubCategory& categoryId = {}) const;

        /// \brief Get the number of rows of the specified sub category
        /// \param categoryId The vector<string> for the subcategory
        size_t size(const SubCategory& categoryId = {}) const;

        /// \brief Get the number of rows of the specified sub category
        std::vector<SubCategory> allSubCategories() const;

        /// \brief Get the map of categories
        inline CategoryMap getCategoryMap() const { return categoryMap_; }

     private:
        /// Category map given (see constructor).
        const CategoryMap categoryMap_;

        /// Map of data for each possible subcategory
        DataSets dataSets_;

        /// \brief Uses category map to generate listings of all possible subcategories.
        void makeDataSets();

        /// \brief Convenience function used to make a string out of a subcategory listing.
        /// \param categoryId Subcategory (ie: vector<string>) listing.
        static std::string makeSubCategoryStr(const SubCategory& categoryId);
    };
}  // namespace Ingester
