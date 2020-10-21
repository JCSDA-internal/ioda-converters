/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>

#include "ioda/ObsGroup.h"

namespace Ingester
{
    class DataObject
    {
     public:
        virtual ioda::Variable createVariable(ioda::ObsGroup obsGroup,
                                              const std::string& name,
                                              const std::vector<ioda::Variable>& dimensions) = 0;
        virtual void print() const = 0;
    };
}  // namespace Ingester


