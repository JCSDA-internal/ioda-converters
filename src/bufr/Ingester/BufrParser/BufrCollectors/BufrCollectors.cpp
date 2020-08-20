/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <memory>

#include "BufrCollectors.h"

#include "IngesterData.h"
#include "BufrRepCollector.h"
#include "BufrIntCollector.h"


using namespace std;
using namespace Ingester;

BufrCollectors::BufrCollectors(unsigned int fileUnit) :
    fileUnit_(fileUnit)
{
}

void BufrCollectors::addMnemonicSets(const vector<BufrMnemonicSet>& mnemonicSets)
{
    for (const auto& set : mnemonicSets)
    {
        addMnemonicSet(set);
    }
}

void BufrCollectors::addMnemonicSet(const BufrMnemonicSet& mnemonicSet)
{
    if (mnemonicSet.getMaxColumn() == 1)
    {
        collectors_.push_back(make_shared<BufrIntCollector>(fileUnit_, mnemonicSet));
    }
    else
    {
        collectors_.push_back(make_shared<BufrRepCollector>(fileUnit_, mnemonicSet));
    }
}

void BufrCollectors::collect()
{
    for (const auto& collector : collectors_)
    {
        collector->collect();
    }
}

shared_ptr<IngesterData> BufrCollectors::finalize()
{
    auto ingesterData = make_shared<IngesterData>();

    for (const auto& collector : collectors_)
    {
        IngesterArrayMap dataMap = collector->finalize();

        for (auto const& pair : dataMap)
        {
            ingesterData->add(pair.first, pair.second);
        }
    }

    return ingesterData;
}
