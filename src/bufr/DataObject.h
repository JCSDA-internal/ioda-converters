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

#include "ioda/ObsGroup.h"
#include "ioda/defs.h"

#include "ResultSet.h"


namespace Ingester
{
    typedef std::vector<size_t> Dimensions;
    typedef Dimensions Location;
    typedef Dimensions Slice;

    class DataObjectBase
    {
    public:
        static std::shared_ptr<DataObjectBase> 
            fromResult(const std::shared_ptr<bufr::ResultBase>& resultBase);

        explicit DataObjectBase(const Dimensions& dims) : dims_(dims) {};
        virtual ~DataObjectBase() = 0;
        Dimensions getDims() const { return dims_; }

        virtual int getAsInt(const Location& loc) const = 0;
        virtual float getAsFloat(const Location& loc) const = 0;
        virtual std::string getAsString(const Location& loc) const = 0;
        virtual std::vector<float> getAsFloatVector() const = 0;

        /// \brief Slice the data object given a vector of row indices.
        /// \param slice The indices to slice.
        /// \return Slice of the data object.
        virtual std::shared_ptr<DataObjectBase> 
            slice(const std::vector<std::size_t>& rows) const = 0;

     protected:
        Dimensions dims_;
    };

    

    /// \brief Abstract base class for intermediate data object that bridges the Parsers with the
    /// IodaEncoder.
    template <typename T>
    class DataObject : public DataObjectBase
    {
     public:

        /// \brief Constructor.
        /// \param dimensions The dimensions of the data object.
        DataObject(const std::vector<T>& data, const Dimensions& dimensions);
        ~DataObject() = default;

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
                                      int compressionLevel) const
        {
            auto params = makeCreationParams(chunks, compressionLevel);
            auto var = obsGroup.vars.createWithScales<T>(name, dimensions, params);
            var.write(data_);
            return var;
        };

        /// \brief Print data to stdout for debug purposes.
        void print() const 
        {
            std::cout << "DataObject: " << data_ << std::endl;
        };

        std::vector<T> getRawData() const { return data_; }

        T get(const Location& loc) const
        {
            //Compute the index into the data array
            size_t index = 0;
            for (auto i = 0; i < loc.size(); ++i)
            {
                index += loc[i] * dims_[i];
            }

            return data_[index];
        };

        int getAsInt(const Location& loc) const final
        {
            int result = 0;
            if (std::is_same<T, float>::value)
            {
                result = static_cast<int>(get(loc));
            }
            else if (std::is_same<T, std::string>::value)
            {
                result = std::stoi(get(loc));
            }

            return result;
        }

        float getAsFloat(const Location& loc) const final
        {
            float result = 0;
            if (std::is_same<T, float>::value)
            {
                result = get(loc);
            }
            else if (std::is_same<T, std::string>::value)
            {
                result = std::stof(get(loc));
            }

            return result;
        }

        std::string getAsString(const Location& loc) const final
        {
            float result = 0;
            if (std::is_same<T, float>::value)
            {
                result = std::to_string(get(loc));
            }
            else if (std::is_same<T, std::string>::value)
            {
                result = get(loc);
            }
        }
        
        virtual std::vector<float> getAsFloatVector() const final
        {
            std::vector<float> result;
            if (std::is_same<T, float>::value)
            {
                result = data_;
            }
            else if (std::is_same<T, std::string>::value)
            {
                result.resize(data_.size());
                for (size_t dataIdx; dataIdx < data_.size(); ++dataIdx)
                {
                    result[dataIdx] = std::stof(data_[dataIdx]);
                }
            }

            return result;
        }

        std::shared_ptr<DataObjectBase> slice(const std::vector<std::size_t>& rows) const final
        {
            //Compute product of extra dimensions)
            std::size_t extraDims = 1;
            for (auto i = 1; i < dims_.size(); ++i)
            {
                extraDims *= dims_[i];
            }

            // Make new DataObject with the rows we want
            std::vector<T> newData;
            newData.resize(dims_[0] * extraDims);
            for (auto i = 0; i < rows.size(); ++i)
            {
                newData.insert(newData.end(), 
                    data_.begin() + rows[i] * extraDims, data_.begin() + (rows[i] + 1) * extraDims);
            }

            auto sliceDims = dims_;
            sliceDims[0] = rows.size();

            return std::make_shared<DataObject<T>>(newData, sliceDims);
        }

     private:
        std::vector<T> data_;

        ioda::VariableCreationParameters makeCreationParams(
                                                    const std::vector<ioda::Dimensions_t>& chunks,
                                                    int compressionLevel) const;

    };
}  // namespace Ingester
