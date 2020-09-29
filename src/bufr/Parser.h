/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "eckit/config/LocalConfiguration.h"

#include "DataContainer.h"

namespace Ingester
{
    class Parser
    {
     public:
        Parser() = default;
        Parser(const eckit::Configuration& conf);

        virtual std::shared_ptr<DataContainer> parse(const size_t maxMsgsToParse = 0) = 0;
        virtual void reset() = 0;
    };
}  // namepace Ingester
