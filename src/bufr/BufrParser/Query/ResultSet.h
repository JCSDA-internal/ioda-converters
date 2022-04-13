/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>


namespace Ingester {
namespace bufr {

    /// \brief The missing data value for all BUFR data.
    const double MissingValue = 10e10;

    /// \brief Represents a single BUFR data element (a element from one message subset). It
    /// contains both the data value(s) and the associated metadata that is used to construct the
    /// results data.
    struct DataField
    {
        std::string name;
        std::string queryStr;
        bool isString;
        bool missing = false;
        std::vector<double> data;
        std::vector<size_t> seqPath;
        std::vector<std::vector<int>> seqCounts;
        std::vector<std::string> dimPaths;
        std::vector<int> exportDims;
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
                if (fields_[fieldIdx].name == name)
                {
                    result = fieldIdx;
                    break;
                }
            }

            return result;
        }

     private:
        std::vector<DataField> fields_;
    };


    /// \brief The base class for all Results.
    struct ResultBase
    {
        std::string field_name;
        std::string group_by_field_name;
        std::vector<int> dims;
        std::vector<std::string> dimPaths;
        std::map<std::string, int> fieldIdxMap_;

        virtual ~ResultBase() {}
        virtual void print() = 0;
    };

    /// \brief The resulting data created by the ResultSet.
    template <typename T>
    struct Result : ResultBase
    {
        typedef T value_type;
        std::vector<T> data;

        /// \brief Print the data to stdout.
        void print() final
        {
            std::cout << data.size() << std::endl;
            for (auto val : data)
            {
                std::cout << val << ", ";
            }
        }
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
        /// \return A Result object containing the data.
        std::shared_ptr<ResultBase> get(const std::string& fieldName,
                                        const std::string& groupByFieldName = "") const;

        /// \brief Adds a new DataFrame to the ResultSet and returns a reference to it.
        /// \return A reference to the new DataFrame.
        DataFrame& nextDataFrame();

        /// \brief Sets the first dataframe attribute to indicate that a DataField is the string
        /// type.
        /// \param fieldIdx The index of the field to set.
        void indicateFieldIsString(int fieldIdx)
        {
            dataFrames_.front().fieldAtIdx(fieldIdx).isString = true;
        }

        /// \brief Checks if a DataField is the string type.
        /// \param fieldIdx The index of the field.
        /// \return True if the field is the string type.
        bool isFieldStr(int fieldIdx)
        {
            return dataFrames_.front().fieldAtIdx(fieldIdx).isString;
        }

     private:
        std::vector<DataFrame> dataFrames_;
        std::vector<std::string> names_;
        std::vector<int> fieldWidths;

        /// \brief Computes the data for a specific field with a given name grouped by the
        /// groupByField. It determines the required dimensions for the field and then uses
        /// getRowsForField to get the relevant data from each DataFrame.
        /// \param fieldName The name of the field to get the data for.
        /// \param groupByFieldName The name of the field to group the data by.
        /// \param dims The size of the dimensions of the result data (any number of dimensions).
        /// \param dimPaths The dimensioning sub-query path strings.
        void getRawValues(const std::string& fieldName,
                          const std::string& groupByField,
                          std::vector<double>& data,
                          std::vector<int>& dims,
                          std::vector<std::string>& dimPaths) const;

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
        bool isString(const std::string& fieldName) const;
    };
}  // namespace bufr
}  // namespace Ingester
