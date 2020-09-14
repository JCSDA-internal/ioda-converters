/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>

#include <memory>

#include "BufrCollectors.h"
#include "BufrIntCollector.h"
#include "BufrRepCollector.h"
#include "DataContainer.h"


namespace Ingester
{
    BufrCollectors::BufrCollectors(unsigned int fileUnit) :
        fileUnit_(fileUnit)
    {
    }

    void BufrCollectors::addMnemonicSets(const std::vector <BufrMnemonicSet> &mnemonicSets)
    {
        for (const auto &set : mnemonicSets)
        {
            addMnemonicSet(set);
        }
    }

    void BufrCollectors::addMnemonicSet(const BufrMnemonicSet &mnemonicSet)
    {
        if (mnemonicSet.getMaxColumn() == 1)
        {
            collectors_.push_back(std::make_shared<BufrIntCollector>(fileUnit_, mnemonicSet));
        }
        else
        {
            collectors_.push_back(std::make_shared<BufrRepCollector>(fileUnit_, mnemonicSet));
        }
    }

    void BufrCollectors::collect()
    {
        for (const auto &collector : collectors_)
        {
            collector->collect();
        }
    }

    BufrDataMap BufrCollectors::finalize()
    {
        auto dataMap = BufrDataMap ();

        for (const auto &collector : collectors_)
        {
            IngesterArrayMap collectorDataMap = collector->finalize();
            dataMap.insert(collectorDataMap.begin(), collectorDataMap.end());
        }

        return dataMap;
    }
}  // namespace Ingester
