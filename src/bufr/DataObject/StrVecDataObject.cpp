//
// Created by Ronald McLaren on 9/11/20.
//

#include <iostream>

#include "StrVecDataObject.h"


namespace Ingester
{
    StrVecDataObject::StrVecDataObject(std::vector<std::string>& strVector) :
        strVector_(strVector)
    {
    }

    ioda::Variable StrVecDataObject::createVariable(ioda::ObsGroup obsGroup,
                                                    std::string name,
                                                    std::vector<ioda::Variable> dimensions)
    {
        static ioda::VariableCreationParameters params = makeCreationParams();

        auto var = obsGroup.vars.createWithScales<std::string>(name, dimensions, params);
        var.write(strVector_);
        return var;
    }

    void StrVecDataObject::print()
    {
        for (const auto& str : strVector_)
        {
            std::cout << str << std::endl;
        }
    }

    ioda::VariableCreationParameters StrVecDataObject::makeCreationParams()
    {
        ioda::VariableCreationParameters params;
        params.chunk = true;
        params.compressWithGZIP();
        params.setFillValue<std::string>("");

        return params;
    }

}  // namespace Ingester
