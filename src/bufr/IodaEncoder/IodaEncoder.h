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

    class IodaEncoder
    {
     public:
        explicit IodaEncoder(const IodaDescription&  description);
        ioda::ObsGroup encode(const std::shared_ptr<DataContainer>& data, bool append=false);

     private:
        const IodaDescription description_;
        const ioda::Engines::BackendNames backendType_;
    };
}  // namespace Ingester
