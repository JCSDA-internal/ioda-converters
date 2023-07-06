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
    /// \brief Base class for all input Parsers
    class Parser
    {
     public:
        Parser() = default;
        explicit Parser(const eckit::Configuration& conf);
        virtual ~Parser() = default;

        /// \brief Parse the input.
        /// \param maxMsgsToParse Messages to parse (0 for everything)
        virtual std::shared_ptr<DataContainer> parse(const size_t maxMsgsToParse = 0) = 0;

        /// \brief Start over from the beginning
        virtual void reset() = 0;
    };
}  // namespace Ingester
