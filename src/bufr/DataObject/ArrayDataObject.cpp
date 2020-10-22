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
                                                   const std::vector<ioda::Variable>& dimensions)
    {
        static ioda::VariableCreationParameters params = makeCreationParams();

        auto var = obsGroup.vars.createWithScales<float>(name, dimensions, params);
        var.writeWithEigenRegular(eigArray_);
        return var;
    }

    void ArrayDataObject::print() const
    {
        std::cout << eigArray_ << std::endl;
    }

    ioda::VariableCreationParameters ArrayDataObject::makeCreationParams()
    {
        ioda::VariableCreationParameters params;
        params.chunk = true;
        params.compressWithGZIP();
        params.setFillValue<float>(-999);

        return params;
    }
}  // namespace Ingester
