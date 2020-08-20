/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <memory>
#include <map>

#include "BufrParser/BufrTypes.h"

namespace Ingester
{
    class BufrMnemonicSet;
    class BufrAccumulator;
    class BufrCollector;
    class IngesterData;

    class BufrCollectors
    {
    public:
        explicit BufrCollectors(unsigned int fileUnit);
        ~BufrCollectors() = default;

        void addMnemonicSets(const std::vector<BufrMnemonicSet>& mnemonicSets);
        void addMnemonicSet(const BufrMnemonicSet& mnemonicSet);
        void collect();
        std::shared_ptr<IngesterData> finalize();

    private:
        unsigned int fileUnit_;
        std::vector<std::shared_ptr<BufrCollector>> collectors_;
    };
}
