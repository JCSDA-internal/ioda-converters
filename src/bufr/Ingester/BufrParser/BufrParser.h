/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <vector>
#include <string>
#include <memory>

#include "Eigen/Dense"

#include "BufrDescription.h"


namespace Ingester
{
    class BufrMnemonicSet;
    class IngesterData;
    
    class BufrParser
    {
    public:
        explicit BufrParser(BufrDescription& description);
        ~BufrParser();
        std::shared_ptr<IngesterData> parse(const size_t maxMsgsToParse=0);

    private:
        BufrDescription description_;
        unsigned int fileUnit_;

        int openBufrFile(const std::string& filepath);
        void closeBufrFile(const int fileUnit);
    };
}
