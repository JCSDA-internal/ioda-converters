//
// Created by Ronald McLaren on 9/11/20.
//

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

        IngesterArray get() const { return eigArray_; }

     private:
        const IngesterArray eigArray_;
        static ioda::VariableCreationParameters makeCreationParams();
    };
}  // namespace Ingester


