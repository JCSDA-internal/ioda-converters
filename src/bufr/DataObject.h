/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <algorithm>
#include <set>
#include <string>
#include <memory>
#include <type_traits>
#include <iostream>
#include <numeric>
#include <limits>
#include <math.h>

#include "eckit/exception/Exceptions.h"

#ifdef BUILD_IODA_BINDING
    #include "ioda/ObsGroup.h"
    #include "ioda/defs.h"
#endif

#ifdef BUILD_PYTHON_BINDING
    #include <pybind11/pybind11.h>
    #include <pybind11/numpy.h>
    #include <pybind11/stl.h>

    namespace py = pybind11;
#endif

#include "BufrParser/Query/Constants.h"
#include "BufrParser/Query/QueryParser.h"
#include "BufrParser/Query/NodeLookupTable.h"


namespace Ingester
{
    typedef std::vector<int> Dimensions;
    typedef Dimensions Location;

#ifdef BUILD_IODA_BINDING
    struct DimensionDataBase
    {
        virtual ~DimensionDataBase() = default;

        std::shared_ptr<ioda::NewDimensionScale_Base> dimScale;

        virtual void write(ioda::Variable& var) = 0;
    };

    template<typename T>
    struct DimensionData : public DimensionDataBase
    {
        std::vector<T> data;

        DimensionData() = delete;

        virtual ~DimensionData() = default;

        explicit DimensionData(size_t size) :
            data(std::vector<T>(size, _default()))
        {
        }

        void write(ioda::Variable& var)
        {
            var.write(data);
        }

     private:
        template<typename U = void>
        T _default(typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr)
        {
            return static_cast<T>(0);
        }

        template<typename U = void>
        T _default(typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr)
        {
            return std::string("");
        }
    };
#endif

    /// \brief Abstract base class for intermediate data object that bridges the Parsers with the
    /// IodaEncoder.
    class DataObjectBase
    {
     public:
        explicit DataObjectBase(const std::string& fieldName,
                                const std::string& groupByFieldName,
                                const Dimensions& dims,
                                const std::string& query,
                                const std::vector<bufr::Query>& dimPaths):

            fieldName_(fieldName),
            groupByFieldName_(groupByFieldName),
            dims_(dims),
            query_(query),
            dimPaths_(dimPaths)
        {};

        DataObjectBase() = default;
        virtual ~DataObjectBase() = default;

        // Setters
        void setFieldName(const std::string& fieldName) { fieldName_ = fieldName; }
        void setGroupByFieldName(const std::string& fieldName) { groupByFieldName_ = fieldName; }
        void setDims(const std::vector<int> dims) { dims_ = dims; }
        void setQuery(const std::string& query) { query_ = query; }
        void setDimPaths(const std::vector<bufr::Query>& dimPaths)
            { dimPaths_ = dimPaths; }
        virtual void setData(const bufr::NodeLookupTable::DataVector& data, double dataMissingValue) = 0;

        // Getters
        std::string getFieldName() const { return fieldName_; }
        std::string getGroupByFieldName() const { return groupByFieldName_; }
        Dimensions getDims() const { return dims_; }
        std::string getPath() const { return query_; }
        std::vector<bufr::Query> getDimPaths() const { return dimPaths_; }

#ifdef BUILD_PYTHON_BINDING
       /// \brief Return a numpy array of the data.
       virtual py::array getNumpyArray() const = 0;
#endif

        bool hasSamePath(const std::shared_ptr<DataObjectBase>& dataObject);

        /// \brief Print the data object to a output stream.
        virtual void print(std::ostream &out) const = 0;

        /// \brief Get the data at the location as an integer.
        /// \return Integer data.
        virtual int getAsInt(const Location& loc) const = 0;

        /// \brief Get the data at the location as an float.
        /// \return Float data.
        virtual float getAsFloat(const Location& loc) const = 0;

        /// \brief Is the element at the location the missing value.
        /// \return bool data.
        virtual bool isMissing(const Location& loc) const = 0;

        /// \brief Get the data at the index as an int.
        /// \return Int data.
        virtual int getAsInt(size_t idx) const = 0;

        /// \brief Get the data at the index as an float.
        /// \return Float data.
        virtual float getAsFloat(size_t idx) const = 0;

        /// \brief Is the element at the index the missing value.
        /// \return bool data.
        virtual bool isMissing(size_t idx) const = 0;

        /// \brief Get the data at the Location as an string.
        /// \return String data.
        virtual std::string getAsString(const Location& loc) const = 0;

        /// \brief Get the size of the data
        /// \return Data size.
        virtual size_t size() const = 0;

#ifdef BUILD_IODA_BINDING
        /// \brief Makes an ioda::Variable and adds it to the given ioda::ObsGroup
        /// \param obsGroup Obsgroup where to add the variable
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

        /// \brief Makes a new dimension scale using this data object as the source
        /// \param name The name of the dimension variable.
        /// \param dimIdx The idx of the data dimension to use.
        virtual std::shared_ptr<DimensionDataBase> createDimensionFromData(
                                                                    const std::string& name,
                                                                    std::size_t dimIdx) const = 0;

        /// \brief Makes a new blank dimension scale with default type.
        /// \param name The name of the dimension variable.
        /// \param dimIdx The idx of the data dimension to use.
        virtual std::shared_ptr<DimensionDataBase> createEmptyDimension(
                                                                  const std::string& name,
                                                                  std::size_t dimIdx) const = 0;
#endif

        /// \brief Slice the data object given a vector of row indices.
        /// \param slice The indices to slice.
        /// \return Slice of the data object.
        virtual std::shared_ptr<DataObjectBase>
            slice(const std::vector<std::size_t>& rows) const = 0;

        /// \brief Multiply the stored values in this data object by a scalar.
        /// \param val Scalar to multiply to the data..
        virtual void multiplyBy(double val) = 0;

        /// \brief Add a scalar to the stored values in this data object.
        /// \param val Scalar to add to the data..
        virtual void offsetBy(double val) = 0;

     protected:
        std::string fieldName_;
        std::string groupByFieldName_;
        Dimensions dims_;
        std::string query_;
        std::vector<bufr::Query> dimPaths_;
    };


    template <typename T>
    class DataObject : public DataObjectBase
    {
     public:
        typedef T value_type;
        static constexpr T missingValue() { return std::numeric_limits<T>::max(); }

        /// \brief Constructor.
        /// \param dimensions The dimensions of the data object.
        DataObject() = default;

        DataObject(const std::vector<T>& data,
                   const std::string& field_name,
                   const std::string& group_by_field_name,
                   const Dimensions& dimensions,
                   const std::string& query,
                   const std::vector<bufr::Query>& dimPaths) :
            DataObjectBase(field_name, group_by_field_name, dimensions, query, dimPaths),
            data_(data)
        {};

        virtual ~DataObject() = default;

        /// \brief Set the data for this object
        /// \param data The data vector
        void setData(const std::vector<T>& data) { data_ = data; }

        /// \brief Set the data for this object
        /// \param data The data vector
        /// \param dataMissingValue The missing value used in the raw data
        void setData(const bufr::NodeLookupTable::DataVector& data, double dataMissingValue) final
        {
            _setData(data, dataMissingValue);
        }

#ifdef BUILD_PYTHON_BINDING
        /// \brief Return a numpy array of the data.
        py::array getNumpyArray() const final
        {
            return _getNumpyArray();
        }

        template<typename U = void>
        py::array _getNumpyArray(
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            // Create the data array
            py::array_t<T> data(dims_);
            T* dataPtr = static_cast<T*>(data.mutable_data());
            std::copy(data_.begin(), data_.end(), dataPtr);

            // Create the mask array
            py::array_t<bool> mask(dims_);
            bool* maskPtr = static_cast<bool*>(mask.mutable_data());
            for (size_t idx = 0; idx < data_.size(); idx++)
            {
                maskPtr[idx] = isMissing(idx);
            }

            // Create a masked array from the data and mask arrays
            py::object numpyModule = py::module::import("numpy");
            py::array maskedArray = numpyModule.attr("ma").attr("masked_array")(data, mask);
            numpyModule.attr("ma").attr("set_fill_value")(maskedArray, missingValue());

            return maskedArray;
        }

        template<typename U = void>
        py::array _getNumpyArray(
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr) const
        {
            py::list pyStrList(data_.size());

            // Convert the std::vector<std::string> into a list of Python Unicode strings
            for (size_t i = 0; i < data_.size(); ++i)
            {
                pyStrList[i] = py::str(data_[i]);
            }

            // Create a NumPy array of Python Unicode strings with the correct dimensions
            py::object numpyModule = py::module::import("numpy");
            py::array data = numpyModule.attr("array")(pyStrList, py::dtype("O"));
            data = data.attr("reshape")(dims_);

            // Create the mask array
            py::array_t<bool> mask(dims_);
            bool* maskPtr = static_cast<bool*>(mask.mutable_data());
            for (size_t idx = 0; idx < data_.size(); idx++)
            {
                maskPtr[idx] = isMissing(idx);
            }

            // Create a masked array from the data and mask arrays
            py::array maskedArray = numpyModule.attr("ma").attr("masked_array")(data, mask);
            numpyModule.attr("ma").attr("set_fill_value")(maskedArray, missingValue());

            return maskedArray;
        }
#endif

#ifdef BUILD_IODA_BINDING
        /// \brief Makes an ioda::Variable and adds it to the given ioda::ObsGroup
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

        /// \brief Makes a new dimension scale using this data object as the source
        /// \param name The name of the dimension variable.
        /// \param dimIdx The idx of the data dimension to use.
        std::shared_ptr<DimensionDataBase> createDimensionFromData(const std::string& name,
                                                                   std::size_t dimIdx) const final
        {
            auto dimData = std::make_shared<DimensionData<T>>(getDims()[dimIdx]);
            dimData->dimScale = ioda::NewDimensionScale<T>(name, getDims()[dimIdx]);

            std::copy(data_.begin(),
                      data_.begin() + dimData->data.size(),
                      dimData->data.begin());

            // Validate this data object is a valid (has values that repeat for each frame
            for (size_t idx = 0; idx < data_.size(); idx += dimData->data.size())
            {
                if (!std::equal(data_.begin(),
                                data_.begin() + dimData->data.size(),
                                data_.begin() + idx,
                                data_.begin() + idx + dimData->data.size()))
                {
                    std::stringstream errStr;
                    errStr << "Dimension " << name << " has an invalid source field. ";
                    errStr << "The values dont repeat in each sequence.";
                    throw eckit::BadParameter(errStr.str());
                }
            }

            return dimData;
        }

        /// \brief Makes a new blank dimension scale with default type.
        /// \param name The name of the dimension variable.
        /// \param dimIdx The idx of the data dimension to use.
        std::shared_ptr<DimensionDataBase> createEmptyDimension(const std::string& name,
                                                                  std::size_t dimIdx) const final
        {
            auto dimData = std::make_shared<DimensionData<int>>(getDims()[dimIdx]);
            dimData->dimScale = ioda::NewDimensionScale<int>(name, getDims()[dimIdx]);
            return dimData;
        }
#endif

        /// \brief Print the data object to a output stream.
        void print(std::ostream &out) const final
        {
            out << "DataObject " << fieldName_ << ":";
            for (auto val = data_.cbegin(); val != data_.cend(); ++val)
            {
                if (val != data_.cbegin()) out << ", ";
                out << *val;
            }

            out << std::endl;
        };

        /// \brief Get the raw data.
        std::vector<T> getRawData() const { return data_; }

        /// \brief Set the raw data.
        void setRawData(std::vector<T> data) { data_ = data; }

        /// \brief Get data associated with a given location.
        /// \param location The location to get data for.
        /// \return The data at the given location.
        T get(const Location& loc) const
        {
            size_t dim_prod = 1;
            for (int dim_idx = dims_.size(); dim_idx > static_cast<int>(loc.size()); --dim_idx)
            {
                dim_prod *= dims_[dim_idx];
            }

            // Compute the index into the data array
            size_t index = 0;
            for (int dim_idx = loc.size() - 1; dim_idx >= 0; --dim_idx)
            {
                index += dim_prod*loc[dim_idx];
                dim_prod *= dims_[dim_idx];
            }

            return data_[index];
        };

        /// \brief Get the size of the data.
        /// \return The size of the data.
        size_t size() const { return data_.size(); }

        /// \brief Get the data at the location as an integer.
        /// \param loc The coordinate for the data point (ex: if data 2d then loc {2,4} gets data
        ///            at that coordinate).
        /// \return Int data.
        int getAsInt(const Location& loc) const final { return _getAsInt(loc); }

        /// \brief Get the data at the location as a float.
        /// \param loc The coordinate for the data point (ex: if data 2d then loc {2,4} gets data
        ///            at that coordinate).
        /// \return Float data.
        float getAsFloat(const Location& loc) const final { return _getAsFloat(loc); }

        /// \brief Get the data at the location as a string.
        /// \param loc The coordinate for the data point (ex: if data 2d then loc {2,4} gets data
        ///            at that coordinate).
        /// \return String data.
        std::string getAsString(const Location& loc) const final { return _getAsString(loc); }

        /// \brief Is the element at the location the missing value.
        /// \param loc The coordinate for the data point (ex: if data 2d then loc {2,4} gets data
        ///            at that coordinate).
        /// \return bool data.
        bool isMissing(const Location& loc) const final
        {
            return get(loc) == missingValue();
        }

        /// \brief Get the data at the index into the internal 1d array as a int. This function
        ///        gives you direct access to the internal data and doesn't account for dimensional
        ///        information (its up to the user).
        /// \param idx The idx into the internal 1d array.
        /// \return Int data.
        int getAsInt(size_t idx) const final { return _getAsInt(idx); }


        /// \brief idx Get the data at the index into the internal 1d array as a float. This
        ///            function gives you direct access to the internal data and doesn't account for
        ///            dimensional information (its up to the user).
        /// \param idx The idx into the internal 1d array.
        /// \return Float data.
        float getAsFloat(const size_t idx) const final { return _getAsFloat(idx); }


        /// \brief idx See if the data at the index into the internal 1d array is missing. This
        ///            function gives you direct access to the internal data and doesn't account for
        ///            dimensional information (its up to the user).
        /// \param idx The idx into the internal 1d array.
        /// \return bool data.
        bool isMissing(const size_t idx) const final
        {
            return data_[idx] == missingValue();
        }


        /// \brief Slice the dta object according to a list of indices.
        /// \param rows The indices to slice the data object by.
        /// \return Sliced DataObject.
        std::shared_ptr<DataObjectBase> slice(const std::vector<std::size_t>& rows) const final
        {
            // Compute product of extra dimensions)
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

#ifdef BUILD_IODA_BINDING
        /// \brief Make the variable creation parameters.
        /// \param chunks The chunk sizes
        /// \param compressionLevel The compression level
        /// \return The variable creation patterns.
        ioda::VariableCreationParameters makeCreationParams(
                const std::vector<ioda::Dimensions_t>& chunks,
                int compressionLevel) const
        {
            return _makeCreationParams(chunks, compressionLevel);
        }


        /// \brief Make the variable creation parameters for numeric data.
        /// \param chunks The chunk sizes
        /// \param compressionLevel The compression level
        /// \return The variable creation patterns.
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
            params.setFillValue<T>(static_cast<T>(missingValue()));

            return params;
        }


        /// \brief Make the variable creation parameters for string data.
        /// \param chunks The chunk sizes
        /// \param compressionLevel The compression level
        /// \return The variable creation patterns.
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
            params.setFillValue<T>(static_cast<T>(std::string("")));

            return params;
        }
#endif

        /// \brief Get the data at the location as a float for numeric data.
        /// \return Float data.
        template<typename U = void>
        float _getAsFloat(const Location& loc,
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            return static_cast<float> (get(loc));
        }

        /// \brief Get the data at the location as a float for string data.
        /// \return Float data.
        template<typename U = void>
        float _getAsFloat(const Location& loc,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr) const
        {
            return std::stof(get(loc));
        }

        /// \brief Get the data at the location as int for numeric data.
        /// \return Int data.
        template<typename U = void>
        int _getAsInt(const Location& loc,
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            return static_cast<int> (get(loc));
        }

        /// \brief Get the data at the location as int for string data.
        /// \return Int data.
        template<typename U = void>
        int _getAsInt(const Location& loc,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr) const
        {
            return std::stoi(get(loc));
        }

        /// \brief Get the data at the location as string for numeric data.
        /// \return string data.
        template<typename U = void>
        std::string _getAsString(const Location& loc,
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            return std::to_string(get(loc));
        }

        /// \brief Get the data at the location as string for string data.
        /// \return string data.
        template<typename U = void>
        std::string _getAsString(const Location& loc,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr) const
        {
            return get(loc);
        }

        /// \brief Get the data at the index as a int for numeric data.
        /// \return Int data.
        template<typename U = void>
        int _getAsInt(size_t idx,
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            return static_cast<int>(data_[idx]);
        }

        /// \brief Get the data at the index as a int for non-numeric data.
        /// \return Int data.
        template<typename U = void>
        int _getAsInt(size_t idx,
            typename std::enable_if<!std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            throw std::runtime_error("The stored value is not a number");
        }

        /// \brief Get the data at the index as a float for numeric data.
        /// \return Float data.
        template<typename U = void>
        float _getAsFloat(size_t idx,
            typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            return static_cast<float>(data_[idx]);
        }

        /// \brief Get the data at the index as a float for non-numeric data.
        /// \return Float data.
        template<typename U = void>
        float _getAsFloat(size_t idx,
            typename std::enable_if<!std::is_arithmetic<T>::value, U>::type* = nullptr) const
        {
            throw std::runtime_error("The stored value was is not a number");
            return 0.0f;
        }

        /// \brief Set the data associated with this data object (numeric DataObject).
        /// \param data - double vector of raw data
        /// \param dataMissingValue - The number that represents missing values within the raw data
        template<typename U = void>
        void _setData(const bufr::NodeLookupTable::DataVector& data,
                      double dataMissingValue,
                      typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr)
        {
            if (std::holds_alternative<std::vector<double>>(data.data))
            {
                auto rawData = data.rawData<std::vector<double>>();
                data_ = std::vector<T>(rawData.begin(), rawData.end());
            }
            else if (std::holds_alternative<std::vector<std::string>>(data.data))
            {
                auto errStr = std::stringstream();
                errStr << "The data type of the data object is not numeric";
                throw eckit::BadValue(errStr.str());
            }

            std::replace(data_.begin(),
                         data_.end(),
                         static_cast<T>(dataMissingValue),
                         missingValue());
        }

        /// \brief Set the data associated with this data object (string DataObject).
        /// \param data - double vector of raw data
        /// \param dataMissingValue - The number that represents missing values within the raw data
        template<typename U = void>
        void _setData(
            const bufr::NodeLookupTable::DataVector& data,
            double dataMissingValue,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr)
        {
            if (std::holds_alternative<std::vector<double>>(data.data))
            {
                data_ = std::vector<std::string>();
                data_.reserve(data.size());
                auto charPtr = reinterpret_cast<const char*>(std::get<std::vector<double>> (data.data).data());
                for (size_t row_idx = 0; row_idx < data.size(); row_idx++)
                {
                    if (std::get<std::vector<double>>(data.data)[row_idx] != dataMissingValue)
                    {
                        std::string str = std::string(
                            charPtr + row_idx * sizeof(double), sizeof(double));

                        // trim trailing whitespace from str
                        str.erase(std::find_if(str.rbegin(), str.rend(),
                                               [](char c){ return !std::isspace(c); }).base(),
                                  str.end());

                        data_.push_back(str);
                    }
                    else
                    {
                        data_.push_back("");
                    }
                }
            }
            else if (std::holds_alternative<std::vector<std::string>>(data.data))
            {
                data_ = std::get<std::vector<std::string>> (data.data);
            }
        }

        /// \brief Multiply the stored values in this data object by a scalar.
        /// \param val Scalar to multiply to the data..
        void multiplyBy(double val) final
        {
            _multiplyBy(val);
        }

        /// \brief Multiply the stored values in this data object by a scalar (numeric version).
        /// \param val Scalar to multiply to the data.
        template<typename U = void>
        void _multiplyBy(double val,
                         typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr)
        {
            if (typeid(T) == typeid(float) ||   // NOLINT
                typeid(T) == typeid(double) ||  // NOLINT
                trunc(val) == val)
            {
                for (size_t i = 0; i < data_.size(); i++)
                {
                    if (data_[i] != missingValue())
                    {
                        data_[i] = static_cast<T>(static_cast<double>(data_[i]) * val);
                    }
                }
            }
            else
            {
                std::ostringstream str;
                str << "Multiplying integer field \"" << fieldName_ << "\" with a non-integer is ";
                str << "illegal. Please convert it to a float or double.";
                throw std::runtime_error(str.str());
            }
        }

        /// \brief Multiply the stored values in this data object by a scalar (string version).
        /// \param val Scalar to multiply to the data.
        template<typename U = void>
        void _multiplyBy(
            double val,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr)
        {
            throw std::runtime_error("Trying to multiply a string by a number");
        }

        /// \brief Add a scalar to the stored values in this data object.
        /// \param val Scalar to add to the data.
        void offsetBy(double val) final
        {
            _offsetBy(val);
        }


        /// \brief Add a scalar to the stored values in this data object (numeric version).
        /// \param val Scalar to add to the data.
        template<typename U = void>
        void _offsetBy(double val,
                       typename std::enable_if<std::is_arithmetic<T>::value, U>::type* = nullptr)
        {
            for (size_t i = 0; i < data_.size(); i++)
            {
                if (data_[i] != missingValue())
                {
                    data_[i] = data_[i] + static_cast<T>(val);
                }
            }
        }

        /// \brief Add a scalar to the stored values in this data object (string version).
        /// \param val Scalar to add to the data.
        template<typename U = void>
        void _offsetBy(
            double val,
            typename std::enable_if<std::is_same<T, std::string>::value, U>::type* = nullptr)
        {
            throw std::runtime_error("Trying to offset a string by a number");
        }
    };
}  // namespace Ingester
