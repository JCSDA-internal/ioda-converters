/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <iostream>
#include <unordered_map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "DataProvider/DataProvider.h"
#include "DataObject.h"
#include "Target.h"
#include "SubsetLookupTable.h"


namespace Ingester {
namespace bufr {

    /// \brief This class acts as the container for all the data that is collected during the
    /// the BUFR querying process. Internally it arranges the data as DataFrames for each message
    /// subset observation. Each DataFrame contains a list of DataFields, one for each named element
    /// that was collected. Because of the way the collection process works, each DataFrame is
    /// organized the same way (indexes of DataFields line up).
    ///
    /// \par The getter functions for the data construct the final output based on the data and
    /// metadata in these DataFields. There are many complications. For one the data may be jagged
    /// (DataFields instances don't necessarily all have the same number of elements [repeated data
    /// could have a different number of repeats per instance]). Another is the application group_by
    /// fields which affect the dimensionality of the data. In order to make the data into
    /// rectangular arrays it may be necessary to strategically fill in missing values so that the
    /// data is organized correctly in each dimension.
    ///
    class ResultSet
    {
     public:
        explicit ResultSet(const Targets& targets);
        ~ResultSet();

        /// \brief Gets the resulting data for a specific field with a given name grouped by the
        /// optional groupByFieldName.
        /// \param fieldName The name of the field to get the data for.
        /// \param groupByFieldName The name of the field to group the data by.
        /// \param overrideType The name of the override type to convert the data to. Possible
        /// values are int, uint, int32, uint32, int64, uint64, float, double
        /// \return A Result object containing the data.
        std::shared_ptr<Ingester::DataObjectBase>
        get(const std::string& fieldName,
            const std::string& groupByFieldName = "",
            const std::string& overrideType = "") const;

        /// \brief Move a new frame into the ResultSet.
        /// \param frame The NodeLookupTable to std::move.
        void addFrame(SubsetLookupTable&& frame)
        {
            frames_.push_back(std::move(frame));
        }

     private:
        Targets targets_;
        std::vector<SubsetLookupTable> frames_;

        /// \brief Computes the data for a specific field with a given name grouped by the
        /// groupByField. It determines the required dimensions for the field and then uses
        /// getRowsForField to get the relevant data from each DataFrame.
        /// \param fieldName The name of the field to get the data for.
        /// \param groupByFieldName The name of the field to group the data by.
        /// \param dims The size of the dimensions of the result data (any number of dimensions).
        /// \param dimPaths The dimensioning sub-query path strings.
        /// \param info The meta data for the element.
        void getRawValues(const std::string& fieldName,
                          const std::string& groupByField,
                          std::vector<double>& data,
                          std::vector<int>& dims,
                          std::vector<Query>& dimPaths,
                          TypeInfo& info) const;

        /// \brief Retrieves the data for the specified target field, one row per message subset.
        /// The dims are used to determine the filling pattern so that that the resulting data can
        /// be reshaped to the dimensions specified.
        /// \param[in] targetField The target field to retrieve.
        /// \param[out] dataRows The data.
        /// \param[in] dims Vector of dimension sizes.
        /// \param[in] groupbyIdx Idx of the group by field (which query component).
        void getRowsForField(const DataField& targetField,
                             std::vector<std::vector<double>>& dataRows,
                             const std::vector<int>& dims,
                             int groupbyIdx) const;

        /// \brief Is the field a string field?
        /// \param fieldName The name of the field.
        std::string unit(const std::string& fieldName) const;

        /// \brief Make an appropriate DataObject for the data considering all the META data
        /// \param fieldName The name of the field to get the data for.
        /// \param groupByFieldName The name of the field to group the data by.
        /// \param info The meta data for the element.
        /// \param overrideType The name of the override type to convert the data to. Possible
        /// values are int, uint, int32, uint32, int64, uint64, float, double
        /// \param data The data
        /// \param dims The dimensioning information
        /// \param dimPaths The sub-query path strings for each dimension.
        /// \return A Result DataObject containing the data.
        std::shared_ptr<DataObjectBase> makeDataObject(
                                const std::string& fieldName,
                                const std::string& groupByFieldName,
                                TypeInfo& info,
                                const std::string& overrideType,
                                const std::vector<double> data,
                                const std::vector<int> dims,
                                const std::vector<Query> dimPaths) const;

        /// \brief Make an appropriate DataObject for data with the TypeInfo
        /// \param info The meta data for the element.
        /// \return A Result DataObject containing the data.
        std::shared_ptr<DataObjectBase> objectByTypeInfo(TypeInfo& info) const;

        /// \brief Make an appropriate DataObject for data with the override type
        /// \param overrideType The meta data for the element.
        /// \return A Result DataObject containing the data.
        std::shared_ptr<DataObjectBase> objectByType(const std::string& overrideType) const;

        /// \brief Utility function that can be used to split a query string into its components.
        /// \param query The query string.
        /// \return A std::string vector to store the components in.
        static std::vector<std::string> splitPath(const std::string& query);
    };
}  // namespace bufr
}  // namespace Ingester
