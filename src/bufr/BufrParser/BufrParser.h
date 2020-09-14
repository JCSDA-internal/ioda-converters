/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>
#include <string>
#include <vector>

#include "Eigen/Dense"

#include "BufrTypes.h"
#include "BufrDescription.h"


namespace Ingester
{
    class BufrMnemonicSet;
    class DataContainer;

    class BufrParser
    {
     public:
        explicit BufrParser(BufrDescription& description);
        ~BufrParser();
        std::shared_ptr<DataContainer> parse(const size_t maxMsgsToParse = 0);
        void reset();

     private:
        BufrDescription description_;
        unsigned int fileUnit_;

        std::shared_ptr<DataContainer> exportData(const BufrDataMap& sourceData);

        void openBufrFile(const std::string& filepath);
        void closeBufrFile();
    };
}  // namespace Ingester
