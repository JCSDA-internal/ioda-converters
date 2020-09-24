/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "DataObject.h"
#include "IngesterTypes.h"


namespace Ingester
{
    class ArrayDataObject : public DataObject
    {
     public:
        explicit ArrayDataObject(const IngesterArray& eigArray);
        ~ArrayDataObject() = default;

        ioda::Variable createVariable(ioda::ObsGroup obsGroup,
                                      std::string name,
                                      std::vector<ioda::Variable> dimensions) final;
        void print() final;

        inline IngesterArray get() const { return eigArray_; }

     private:
        const IngesterArray eigArray_;
        static ioda::VariableCreationParameters makeCreationParams();
    };
}  // namespace Ingester


