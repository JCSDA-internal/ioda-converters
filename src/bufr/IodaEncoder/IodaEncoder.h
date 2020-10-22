/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>

#include "IodaDescription.h"

#include "ioda/Group.h"
#include "ioda/Engines/Factory.h"
#include "ioda/ObsGroup.h"


namespace Ingester
{
    class DataContainer;

    /// \brief Uses IodaDescription and parsed data to create IODA data.
    class IodaEncoder
    {
     public:
        explicit IodaEncoder(const IodaDescription&  description);

        /// \brief Encode the data into an ioda::ObsGroup object
        ioda::ObsGroup encode(const std::shared_ptr<DataContainer>& data,
                              bool append = false);

     private:
        /// \brief The description
        const IodaDescription description_;

        /// \brief The backend type to usee
        const ioda::Engines::BackendNames backendType_;
    };
}  // namespace Ingester
