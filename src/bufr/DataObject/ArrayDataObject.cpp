/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>

#include "ArrayDataObject.h"

namespace Ingester
{
    ArrayDataObject::ArrayDataObject(const IngesterArray& eigArray) :
        eigArray_(eigArray)
    {
    }

    ioda::Variable ArrayDataObject::createVariable(ioda::ObsGroup& obsGroup,
                                                   const std::string& name,
                                                   const std::vector<ioda::Variable>& dimensions,
                                                   const std::vector<ioda::Dimensions_t>& chunks,
                                                   int compressionLevel)
    {
        auto params = makeCreationParams(chunks, compressionLevel);
        auto var = obsGroup.vars.createWithScales<FloatType>(name, dimensions, params);
        var.writeWithEigenRegular(eigArray_);
        return var;
    }

    void ArrayDataObject::print() const
    {
        std::cout << eigArray_ << std::endl;
    }

    size_t ArrayDataObject::nrows() const
    {
        return eigArray_.rows();
    }

    size_t ArrayDataObject::ncols() const
    {
        return eigArray_.cols();
    }

    ioda::VariableCreationParameters ArrayDataObject::makeCreationParams(
                                                    const std::vector<ioda::Dimensions_t>& chunks,
                                                    int compressionLevel)
    {
        ioda::VariableCreationParameters params;
        params.chunk = true;
        params.chunks = chunks;
        params.compressWithGZIP(compressionLevel);
        params.setFillValue<FloatType>(-999);

        return params;
    }

    float ArrayDataObject::getFloat(size_t row, size_t col) const
    {
        return eigArray_(row, col);
    }

    std::string ArrayDataObject::getString(size_t row, size_t col) const
    {
        return std::to_string(eigArray_(row, col));
    }

    int ArrayDataObject::getInt(size_t row, size_t col) const
    {
        return static_cast<int>(eigArray_(row, col));
    }

    std::vector<float> ArrayDataObject::getFloats(size_t col) const
    {
        std::vector<float> result;
        for (size_t row = 0; row < eigArray_.rows(); ++row)
        {
            result.push_back(eigArray_(row, col));
        }

        return result;
    }

    std::vector<std::string> ArrayDataObject::getStrings(size_t col) const
    {
        std::vector<std::string> result;
        for (size_t row = 0; row < eigArray_.rows(); ++row)
        {
            result.push_back(std::to_string(eigArray_(row, col)));
        }

        return result;
    }

    std::vector<int> ArrayDataObject::getInts(size_t col) const
    {
        std::vector<int> result;
        for (size_t row = 0; row < eigArray_.rows(); ++row)
        {
            result.push_back(static_cast<int>(eigArray_(row, col)));
        }

        return result;
    }

    std::shared_ptr<DataObject> ArrayDataObject::slice(const std::vector<size_t>& rowIndices) const
    {
        IngesterArray result(rowIndices.size(), eigArray_.cols());

        for (size_t rowIdx = 0; rowIdx < rowIndices.size(); rowIdx++)
        {
            result.row(rowIdx) = eigArray_.row(static_cast<Eigen::Index>(rowIndices[rowIdx]));
        }

        return std::make_shared<ArrayDataObject>(result);
    }
}  // namespace Ingester
