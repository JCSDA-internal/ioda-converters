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
    /// \brief Abstract base class for intermediate data object that bridges the Parsers with the
    /// IodaEncoder.
    class DataObject
    {
     public:
        /// \brief Makes an ioda::Variable and ads it to the given ioda::ObsGroup
        /// \param obsGroup Obsgroup were to add the variable
        /// \param name The name to associate with the variable (ex "latitude@MetaData")
        /// \param dimensions List of Variables to use as the dimensions for this new variable
        virtual ioda::Variable createVariable(ioda::ObsGroup& obsGroup,
                                              const std::string& name,
                                              const std::vector<ioda::Variable>& dimensions) = 0;

        /// \brief Print data to stdout for debug purposes.
        virtual void print() const = 0;
    };
}  // namespace Ingester


