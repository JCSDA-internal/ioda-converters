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

#ifdef BUILD_PYTHON_BINDING
    #include <pybind11/pybind11.h>
    #include <pybind11/numpy.h>
    #include <pybind11/stl.h>
    #include <pybind11/stl_bind.h>

    namespace py = pybind11;
#endif

#include "DataProvider/DataProvider.h"
#include "DataObject.h"
#include "Target.h"
#include "SubsetLookupTable.h"
#include "Data.h"


namespace Ingester {
namespace bufr {

namespace details
{
    struct TargetMetaData
    {
        size_t targetIdx;
        TypeInfo typeInfo;
        std::vector<int> dims = {0};
        std::vector<int> rawDims = {0};
        std::vector<int> filteredDims = {0};
        std::vector<int> groupedDims = {};
        std::vector<char> missingFrames;
        std::vector<Query> dimPaths;
    };

    struct ResultData
    {
        Data buffer;
        std::vector<int> dims;
        std::vector<int> rawDims;
        std::vector<Query> dimPaths;
    };

    typedef std::shared_ptr<TargetMetaData> TargetMetaDataPtr;

}  // namespace details

    typedef SubsetLookupTable Frame;
    typedef std::vector<Frame> Frames;

    /// \brief This class acts as the container for all the data that is collected during the
    /// the BUFR querying process in the form of SubsetLookupTable instances.
    ///
    /// \par The getter functions for the data construct the final output based on the data and
    /// metadata in these lookup tables. There are many complications. For one the data may be
    /// jagged (lookup table instances don't necessarily all have the same number of elements
    /// [repeated data could have a different number of repeats per instance]). Another is the
    /// application group_by fields which affect the dimensionality of the data. In order to make
    /// the data into rectangular arrays it may be necessary to strategically fill in missing values
    /// so that the data is organized correctly in each dimension.
    ///
    class ResultSet
    {
     public:
        ResultSet() = default;
        ~ResultSet() = default;

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
        void addFrame(Frame&& frame)
        {
            frames_.push_back(std::move(frame));
        }

#ifdef BUILD_PYTHON_BINDING
        /// \brief Gets a numpy array for the resulting data for a specific field with a given
        /// name grouped by the optional groupByFieldName.
        /// \param fieldName The name of the field to get the data for.
        /// \param groupByFieldName The name of the field to group the data by.
        /// \param type The name of the type to convert the data to. Possible values are int, uint,
        /// int32, uint32, int64, uint64, float, double
        py::array getNumpyArray(const std::string& fieldName,
                                const std::string& groupByFieldName = "",
                                const std::string& overrideType = "") const;

        /// \brief Gets a numpy array of datetime objects for the resulting data for a specific
        /// field with a given name grouped by the optional groupByFieldName.
        /// \param year The name of the field to use for the year.
        /// \param month The name of the field to use for the month.
        /// \param day The name of the field to use for the day.
        /// \param hour The name of the field to use for the hour.
        /// \param minute (Optional) The name of the field to use for the minute.
        /// \param second (Optional) The name of the field to use for the second.
        /// \param groupBy (Optional) The name of the field to group the data by.
        py::array getNumpyDatetimeArray(const std::string& year,
                                        const std::string& month,
                                        const std::string& day,
                                        const std::string& hour,
                                        const std::string& minute = "",
                                        const std::string& second = "",
                                        const std::string& groupBy = "") const;
#endif

     private:
        Frames frames_;

        /// \brief Computes and returns metadata associated with a target.
        /// \param name The name of the target to get the metadata for.
        /// \return A TargetMetaData object containing the metadata.
        details::TargetMetaDataPtr analyzeTarget(const std::string& name) const;

        /// \brief Assembles the data fragments for a target into a single ResultData object.
        /// \param targetMetaData The metadata for the target to assemble the data for.
        /// \return A ResultData object containing the data.
        details::ResultData assembleData(const details::TargetMetaDataPtr& targetMetaData) const;

        /// \brief Copies the data from a frame into a ResultData object.
        /// \param data The ResultData object to copy the data into.
        /// \param frame The frame to copy the data from.
        /// \param target The target to copy the data for.
        /// \param outputOffset The offset into the ResultData object to copy the data to.
        void copyData(details::ResultData& data,
                      const Frame& frame,
                      const TargetPtr& target,
                      size_t outputOffset) const;

        /// \brief Copies the data from a frame into a ResultData object.
        /// \param data The ResultData object to copy the data into.
        /// \param frame The frame to copy the data from.
        /// \param target The target to copy the data for.
        /// \param outputOffset The offset into the ResultData object to copy the data to.
        /// \param inputOffset The offset into the frame to copy the data from.
        /// \param dimIdx The index of the dimension to copy the data for.
        /// \param countNumber The current count
        /// \param countOffset The offset into the count array.
        void _copyData(details::ResultData& data,
                       const Frame& frame,
                       const TargetPtr& target,
                       size_t& outputOffset,
                       size_t& inputOffset,
                       const size_t dimIdx,
                       const size_t countNumber,
                       const size_t countOffset) const;

        /// \brief Validates that the group_by field is valid for the target. Throws an exception if
        ///        it is not.
        /// \param targetMetaData The metadata for the target.
        /// \param groupByMetaData The metadata for the group_by field.
        void validateGroupByField(const details::TargetMetaDataPtr& targetMetaData,
                                  const details::TargetMetaDataPtr& groupByMetaData) const;


        /// \brief Copies filtered data from a source ResultData object into a destination
        ///        ResultData object.
        /// \param resData The ResultData object to copy the data into.
        /// \param srcData The ResultData object to copy the data from.
        /// \param target The target to copy the data for.
        /// \param inputOffset The offset into the source ResultData object to copy the data from.
        /// \param outputOffset The offset into the destination ResultData object to copy the data
        ///        to.
        /// \param depth The depth of the dimension to copy the data for.
        /// \param skipResult Whether to skip copying the result data.
        void copyFilteredData(details::ResultData& resData,
                              const details::ResultData& srcData,
                              const TargetPtr& target,
                              size_t& inputOffset,
                              size_t& outputOffset,
                              size_t depth,
                              size_t maxDepth,
                              bool skipResult) const;

        /// \brief Modify the ResultData object to apply the group_by field.
        /// \param resData The ResultData object to modify.
        /// \param targetMetaData The metadata for the target.
        /// \param groupByFieldName The name of the field to group the data by.
        void applyGroupBy(details::ResultData& resData,
                          const details::TargetMetaDataPtr& targetMetaData,
                          const std::string& groupByFieldName) const;

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
                                const TypeInfo& info,
                                const std::string& overrideType,
                                const Data& data,
                                const std::vector<int>& dims,
                                const std::vector<Query>& dimPaths) const;

        /// \brief Make an appropriate DataObject for data with the TypeInfo
        /// \param info The meta data for the element.
        /// \return A Result DataObject containing the data.
        std::shared_ptr<DataObjectBase> objectByTypeInfo(const TypeInfo& info) const;

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
