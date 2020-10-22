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
    /// \brief Container for data that can be expressed as lists of strings
    class StrVecDataObject : public DataObject
    {
     public:
        explicit StrVecDataObject(std::vector<std::string>& strVector);
        ~StrVecDataObject() = default;

        /// \brief Makes an ioda::Variable and ads it to the given ioda::ObsGroup
        /// \param obsGroup Obsgroup were to add the variable
        /// \param name The name to associate with the variable (ex "latitude@MetaData")
        /// \param dimensions List of Variables to use as the dimensions for this new variable
        ioda::Variable createVariable(ioda::ObsGroup& obsGroup,
                                      const std::string& name,
                                      const std::vector<ioda::Variable>& dimensions) final;

        /// \brief Print data to stdout for debug purposes.
        void print() const final;

        // Getters
        inline std::vector<std::string> get() const { return strVector_; }

     private:
        /// \brief The data
        const std::vector<std::string> strVector_;

        /// \brief Create an ioda::VariableCreationParameters for the data
        static ioda::VariableCreationParameters makeCreationParams();
    };
}  // namespace Ingester


