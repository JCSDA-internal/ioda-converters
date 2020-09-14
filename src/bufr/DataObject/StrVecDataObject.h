//
// Created by Ronald McLaren on 9/11/20.
//

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


