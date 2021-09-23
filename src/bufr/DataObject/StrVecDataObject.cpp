/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>

#include "StrVecDataObject.h"

#include "eckit/exception/Exceptions.h"


namespace Ingester
{
    StrVecDataObject::StrVecDataObject(const std::vector<std::string>& strVector) :
        strVector_(strVector)
    {
    }

    ioda::Variable StrVecDataObject::createVariable(ioda::ObsGroup& obsGroup,
                                                    const std::string& name,
                                                    const std::vector<ioda::Variable>& dimensions,
                                                    const std::vector<ioda::Dimensions_t>& chunks,
                                                    int compressionLevel)
    {
        auto params = makeCreationParams(chunks, compressionLevel);
        auto var = obsGroup.vars.createWithScales<std::string>(name, dimensions, params);
        var.write(strVector_);
        return var;
    }

    void StrVecDataObject::print() const
    {
        for (const auto& str : strVector_)
        {
            std::cout << str << std::endl;
        }
    }

    size_t StrVecDataObject::nrows() const
    {
        return strVector_.size();
    }

    size_t StrVecDataObject::ncols() const
    {
        return 1;
    }

    ioda::VariableCreationParameters StrVecDataObject::makeCreationParams(
                                                    const std::vector<ioda::Dimensions_t>& chunks,
                                                    int compressionLevel)
    {
        ioda::VariableCreationParameters params;
        params.chunk = true;
        params.chunks = chunks;
        params.compressWithGZIP(compressionLevel);

        return params;
    }

    std::string StrVecDataObject::getString(size_t row, size_t col) const
    {
        return strVector_[row];
    }

    float StrVecDataObject::getFloat(size_t row, size_t col) const
    {
        float result = 0.0;

        if (std::find_if(strVector_[row].begin(), strVector_[row].end(),
                         [](char c) { return !std::isdigit(c); }) == strVector_[row].end())
        {
            result = std::stof(strVector_[row]);
        }
        else
        {
            throw eckit::BadParameter("DataObject: Could not parse float from string: " + strVector_[row]);
        }

        return result;
    }

    int StrVecDataObject::getInt(size_t row, size_t col) const
    {
        int result = 0;
        if (std::find_if(strVector_[row].begin(), strVector_[row].end(),
                         [](char c) { return !std::isdigit(c); }) == strVector_[row].end())
        {
            result = std::stoi(strVector_[row]);
        }
        else
        {
            throw eckit::BadParameter("DataObject: Could not parse int from string: " + strVector_[row]);
        }

        return result;
    }

    std::vector<float> StrVecDataObject::getFloats(size_t col) const
    {
        std::vector<float> result;
        for (const auto& num : getInts())
        {
            result.push_back(static_cast<float>(num));
        }

        return result;
    }

    std::vector<std::string> StrVecDataObject::getStrings(size_t col) const
    {
        return strVector_;
    }

    std::vector<int> StrVecDataObject::getInts(size_t col) const
    {
        std::vector<int> result;
        for (const auto& str : strVector_)
        {
            int value = 0;
            if (std::find_if(str.begin(), str.end(),
                             [](char c) { return !std::isdigit(c); }) == str.end())
            {
                value = std::stoi(str);
            }
            else
            {
                throw eckit::BadParameter("DataObject: Could not parse int from string: " + str);
            }

            result.push_back(value);
        }

        return result;
    }

    std::shared_ptr<DataObject> StrVecDataObject::slice(const std::vector<size_t>& rowIndices) const
    {
        std::vector<std::string> newStrVector;

        for (const auto& rowIndex : rowIndices)
        {
            newStrVector.push_back(strVector_[rowIndex]);
        }

        return std::make_shared<StrVecDataObject>(newStrVector);
    }
}  // namespace Ingester
