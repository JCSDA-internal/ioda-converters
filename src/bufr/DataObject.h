/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <set>
#include <string>
#include <memory>
#include <type_traits>
#include <iostream>

#include "ioda/ObsGroup.h"
#include "ioda/defs.h"

#include "ResultSet.h"


namespace Ingester
{
    typedef std::vector<size_t> Dimensions;
    typedef Dimensions Location;

    const float MissingValue = 10e10;

    /// \brief Abstract base class for intermediate data object that bridges the Parsers with the
    /// IodaEncoder.
    class DataObjectBase
    {
    public:
        static std::shared_ptr<DataObjectBase>
            fromResult(const std::shared_ptr<bufr::ResultBase>& resultBase, const std::string& query);

        explicit DataObjectBase(const std::string& fieldName,
                                const std::string& groupByFieldName,
                                const Dimensions& dims,
                                const std::string& query,
                                const std::vector<std::string>& dimPaths) :

            fieldName_(fieldName),
            groupByFieldName_(groupByFieldName),
            dims_(dims),
            query_(query),
            dimPaths_(dimPaths)
        {};

        virtual ~DataObjectBase() = default;
        std::string getFieldName() const { return fieldName_; }
        std::string getGroupByFieldName() const { return groupByFieldName_; }
        Dimensions getDims() const { return dims_; }
        std::string getPath() const { return query_; }
        std::vector<std::string> getDimPaths() const { return dimPaths_; }

        virtual void print() const = 0;
        virtual int getAsInt(const Location& loc) const = 0;
        virtual float getAsFloat(const Location& loc) const = 0;
        virtual std::string getAsString(const Location& loc) const = 0;
        virtual std::vector<float> getAsFloatVector() const = 0;

        /// \brief Makes an ioda::Variable and ads it to the given ioda::ObsGroup
        /// \param obsGroup Obsgroup were to add the variable
        /// \param name The name to associate with the variable (ex "latitude@MetaData")
        /// \param dimensions List of Variables to use as the dimensions for this new variable
        /// \param chunks List of integers specifying the chunking dimensions
        /// \param compressionLevel The GZip compression level to use, must be 0-9
        virtual ioda::Variable createVariable(
                                      ioda::ObsGroup& obsGroup,
                                      const std::string& name,
                                      const std::vector<ioda::Variable>& dimensions,
                                      const std::vector<ioda::Dimensions_t>& chunks,
                                      int compressionLevel) const = 0;

        /// \brief Slice the data object given a vector of row indices.
        /// \param slice The indices to slice.
        /// \return Slice of the data object.
        virtual std::shared_ptr<DataObjectBase> 
            slice(const std::vector<std::size_t>& rows) const = 0;

     protected:
        std::string fieldName_;
        std::string groupByFieldName_;
        Dimensions dims_;
        std::string query_;
        std::vector<std::string> dimPaths_;
    };


    template <typename T>
    class DataObject : public DataObjectBase
    {
     public:
        typedef T value_type;

        /// \brief Constructor.
        /// \param dimensions The dimensions of the data object.
        DataObject(const std::vector<T>& data,
                   const std::string& field_name,
                   const std::string& group_by_field_name,
                   const Dimensions& dimensions,
                   const std::string& query,
                   const std::vector<std::string>& dimPaths) :
            DataObjectBase(field_name, group_by_field_name, dimensions, query, dimPaths),
            data_(data)
        {};

        ~DataObject() = default;

        /// \brief Makes an ioda::Variable and ads it to the given ioda::ObsGroup
        /// \param obsGroup Obsgroup were to add the variable
        /// \param name The name to associate with the variable (ex "latitude@MetaData")
        /// \param dimensions List of Variables to use as the dimensions for this new variable
        /// \param chunks List of integers specifying the chunking dimensions
        /// \param compressionLevel The GZip compression level to use, must be 0-9
        ioda::Variable createVariable(ioda::ObsGroup& obsGroup,
                                      const std::string& name,
                                      const std::vector<ioda::Variable>& dimensions,
                                      const std::vector<ioda::Dimensions_t>& chunks,
                                      int compressionLevel) const final
        {
            auto params = makeCreationParams(chunks, compressionLevel);
            auto var = obsGroup.vars.createWithScales<T>(name, dimensions, params);
            var.write(data_);
            return var;
        };

        /// \brief Print data to stdout for debug purposes.
        void print() const final
        {
            std::cout << "DataObject " << fieldName_ << ":";

            for (auto element : data_)
            {
                std::cout << element << ", ";
            }

            std::cout << std::endl;
        };

        std::vector<T> getRawData() const { return data_; }
        void setRawData(std::vector<T> data) { data_ = data; }

        T get(const Location& loc) const
        {
            size_t dim_prod = 1;
            for (int dim_idx = dims_.size(); dim_idx > static_cast<int>(loc.size()); --dim_idx)
            {
                dim_prod *= dims_[dim_idx];
            }

            //Compute the index into the data array
            size_t index = 0;
            for (int dim_idx = loc.size() - 1; dim_idx >= 0; --dim_idx)
            {
                index += dim_prod*loc[dim_idx];
                dim_prod *= dims_[dim_idx];
            }

            return data_[index];
        };

        int getAsInt(const Location& loc) const final { return _getAsInt(loc); }
        float getAsFloat(const Location& loc) const final { return _getAsFloat(loc); }
        std::string getAsString(const Location& loc) const final { return _getAsString(loc); }
        std::vector<float> getAsFloatVector() const final { return _getAsFloatVector(); }

        template<typename U = void>
        float _getAsFloat(const Location& loc,
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            return static_cast<float> (get(loc));
        }

        template<typename U = void>
        float _getAsFloat(const Location& loc,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr) const
        {
            return std::stof(get(loc));
        }

        template<typename U = void>
        int _getAsInt(const Location& loc,
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            return static_cast<int> (get(loc));
        }

        template<typename U = void>
        int _getAsInt(const Location& loc,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr) const
        {
            return std::stoi(get(loc));
        }

        template<typename U = void>
        std::string _getAsString(const Location& loc,
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            return std::to_string(get(loc));
        }

        template<typename U = void>
        std::string _getAsString(const Location& loc,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr) const
        {
            return get(loc);
        }

        template<typename U = void>
        std::vector<float> _getAsFloatVector(
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            return data_;
        }

        template<typename U = void>
        std::vector<float> _getAsFloatVector(
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr) const
        {
            std::vector<float> result;

            result.resize(data_.size());
            for (size_t dataIdx; dataIdx < data_.size(); ++dataIdx)
            {
                result[dataIdx] = std::stof(data_[dataIdx]);
            }
            
            return result;
        }

        std::shared_ptr<DataObjectBase> slice(const std::vector<std::size_t>& rows) const final
        {
            //Compute product of extra dimensions)
            std::size_t extraDims = 1;
            for (std::size_t i = 1; i < dims_.size(); ++i)
            {
                extraDims *= dims_[i];
            }

            // Make new DataObject with the rows we want
            std::vector<T> newData;
            newData.reserve(rows.size() * extraDims);
            for (std::size_t i = 0; i < rows.size(); ++i)
            {
                newData.insert(newData.end(),
                               data_.begin() + rows[i] * extraDims,
                               data_.begin() + (rows[i] + 1) * extraDims);
            }

            auto sliceDims = dims_;
            sliceDims[0] = rows.size();

            return std::make_shared<DataObject<T>>(newData,
                                                   fieldName_,
                                                   groupByFieldName_,
                                                   sliceDims,
                                                   query_,
                                                   dimPaths_);
        }

     private:
        std::vector<T> data_;

        template<typename U = void>
        ioda::VariableCreationParameters _makeCreationParams(
            const std::vector<ioda::Dimensions_t>& chunks,
            int compressionLevel,
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            ioda::VariableCreationParameters params;
            params.chunk = true;
            params.chunks = chunks;
            params.compressWithGZIP(compressionLevel);
            params.setFillValue<T>(static_cast<T>(MissingValue));

            return params;
        }

        template<typename U = void>
        ioda::VariableCreationParameters _makeCreationParams(
            const std::vector<ioda::Dimensions_t>& chunks,
            int compressionLevel,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr) const
        {
            ioda::VariableCreationParameters params;
            params.chunk = true;
            params.chunks = chunks;
            params.compressWithGZIP(compressionLevel);

            return params;
        }

        ioda::VariableCreationParameters makeCreationParams(
                                                    const std::vector<ioda::Dimensions_t>& chunks,
                                                    int compressionLevel) const
        {
            return _makeCreationParams(chunks, compressionLevel);
        }
    };
}  // namespace Ingester
