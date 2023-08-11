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
#include "Data.h"


namespace Ingester {
namespace bufr {

    typedef std::vector<std::vector<int>> SeqCounts;

    /// \brief Represents a single BUFR data element (a element from one message subset). It
    /// contains both the data value(s) and the associated metadata that is used to construct the
    /// results data.
    struct DataField
    {
        std::shared_ptr<Target> target;
        Data data;
        SeqCounts seqCounts;
    };

    /// \brief Container for a "row" of data (all the collected data for a message subset)., with a
    /// DataField for each data element
    class DataFrame
    {
     public:
        explicit DataFrame(int fieldCnt)
        {
            fields_.resize(fieldCnt);
        }

        /// \brief Get a reference for the const DataField at the given index.
        /// \param idx The index of the data field to get.
        inline const DataField& fieldAtIdx(size_t idx) const { return fields_[idx]; }

        /// \brief Get a reference for the DataField at the given index.
        /// \param idx The index of the data field to get.
        inline DataField& fieldAtIdx(size_t idx) { return fields_[idx]; }

        /// \brief Get the index for the field with the given name. This field idx is valid for all
        /// data frames in the result set.
        /// \param name The name of the field to get the index for.
        int fieldIndexForNodeNamed(const std::string& name) const
        {
            auto result = -1;
            for (size_t fieldIdx = 0; fieldIdx < fields_.size(); fieldIdx++)
            {
                if (fields_[fieldIdx].target->name == name)
                {
                    result = fieldIdx;
                    break;
                }
            }

            return result;
        }

        /// \brief Check the availability of the field with the given name.
        /// \param name The name of the field to check.
        bool hasFieldNamed(const std::string& name) const
        {
            return fieldIndexForNodeNamed(name) != -1;
        }

     private:
        std::vector<DataField> fields_;
    };

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
        explicit ResultSet(const std::vector<std::string>& names);
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

        /// \brief Adds a new DataFrame to the ResultSet and returns a reference to it.
        /// \return A reference to the new DataFrame.
        DataFrame& nextDataFrame();

        void setTargets(Targets targets) { targets_ = targets; }

     private:
        Targets targets_;
        std::vector<DataFrame> dataFrames_;
        std::vector<std::string> names_;
        std::vector<int> fieldWidths;

        /// \brief Computes the data for a specific field with a given name grouped by the
        /// groupByField. It determines the required dimensions for the field and then uses
        /// getRowsForField to get the relevant data from each DataFrame.
        /// \param fieldName The name of the field to get the data for.
        /// \param groupByFieldName The name of the field to group the data by.
        /// \param[out] data The output data.
        /// \param[out] dims The size of the dimensions of the result data.
        /// \param[out] dimPaths The dimensioning sub-query path strings.
        /// \param[out] info The meta data for the element.
        void getRawValues(const std::string& fieldName,
                          const std::string& groupByField,
                          Data& data,
                          std::vector<int>& dims,
                          std::vector<Query>& dimPaths,
                          TypeInfo& info) const;

        /// \brief Retrieves the data for the specified target field, one row per message subset.
        /// The dims are used to determine the filling pattern so that that the resulting data can
        /// be reshaped to the dimensions specified.
        /// \param[in] targetField The target field to retrieve.
        /// \param[out] dataRows The output data.
        /// \param[in] dims Vector of dimension sizes.
        /// \param[in] groupbyIdx Idx of the group by field (which query component).
        void getRowsForField(const DataField& targetField,
                             std::vector<Data>& dataRows,
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
                                const Data data,
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
