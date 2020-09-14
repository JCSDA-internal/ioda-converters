//
// Created by Ronald McLaren on 9/11/20.
//

#pragma once

#include <string>

#include "ioda/ObsGroup.h"

namespace Ingester
{
    class DataObject
    {
     public:

        virtual ioda::Variable createVariable(ioda::ObsGroup obsGroup,
                                              std::string name,
                                              std::vector<ioda::Variable> dimensions) = 0;
        virtual void print() = 0;

    };
}  // namespace Ingester


