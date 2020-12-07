/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "Filter.h"

#include <string>

#include "BufrParser/BufrTypes.h"


namespace Ingester
{
    class LesserFilter : public Filter
    {
    public:
        LesserFilter(const std::string& mnemonic, float value);

        void apply(BufrDataMap& dataMap) final;

    private:
        const std::string mnemonic_;
        const float value_;
    };
}  // namespace Ingester

