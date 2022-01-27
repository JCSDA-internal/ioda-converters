/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <climits>
#include <iostream>

#include "Int64VecDataObject.h"


namespace Ingester
{
    Int64VecDataObject::Int64VecDataObject(const std::vector<int64_t>& int64Vector) :
        int64Vector_(int64Vector)
    {
    }

    ioda::Variable Int64VecDataObject::createVariable(ioda::ObsGroup& obsGroup,
                                                    const std::string& name,
                                                    const std::vector<ioda::Variable>& dimensions,
                                                    const std::vector<ioda::Dimensions_t>& chunks,
                                                    int compressionLevel)
    {
        auto params = makeCreationParams(chunks, compressionLevel);
        auto var = obsGroup.vars.createWithScales<int64_t>(name, dimensions, params);
        var.write(int64Vector_);
        return var;
    }

    void Int64VecDataObject::print() const
    {
        for (const auto& val : int64Vector_)
        {
            std::cout << "Value: " << val << std::endl;
        }
    }

    size_t Int64VecDataObject::nrows() const
    {
        return int64Vector_.size();
    }

    size_t Int64VecDataObject::ncols() const
    {
        return 1;
    }

    ioda::VariableCreationParameters Int64VecDataObject::makeCreationParams(
                                                    const std::vector<ioda::Dimensions_t>& chunks,
                                                    int compressionLevel)
    {
        ioda::VariableCreationParameters params;
        params.chunk = true;
        params.chunks = chunks;
        params.compressWithGZIP(compressionLevel);
        params.setFillValue<Int64Type>(INT_MIN);

        return params;
    }
}  // namespace Ingester
