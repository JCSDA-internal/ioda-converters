/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "DataObject.h"

#include <string>
#include <vector>


namespace Ingester
{
    class StrVecDataObject : public DataObject
    {
     public:
        explicit StrVecDataObject(std::vector<std::string>& strVector);
        ~StrVecDataObject() = default;

        ioda::Variable createVariable(ioda::ObsGroup obsGroup,
                                      std::string name,
                                      std::vector<ioda::Variable> dimensions) final;
        void print() final;

        std::vector<std::string> get() const { return strVector_; }

     private:
        const std::vector<std::string> strVector_;
        static ioda::VariableCreationParameters makeCreationParams();
    };
}  // namespace Ingester


