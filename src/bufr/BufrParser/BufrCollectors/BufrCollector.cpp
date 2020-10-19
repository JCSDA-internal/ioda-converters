/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BufrCollector.h"


namespace Ingester
{
    BufrCollector::BufrCollector(const int fileUnit, const BufrMnemonicSet mnemonicSet) :
        fileUnit_(fileUnit),
        accumulator_(BufrAccumulator(mnemonicSet.getSize() * mnemonicSet.getMaxColumn())),
        mnemonicSet_(mnemonicSet)
    {
    }

    BufrDataMap BufrCollector::finalize()
    {
        IngesterArrayMap dataMap;
        size_t fieldIdx = 0;
        for (const auto &fieldName : mnemonicSet_.getMnemonics())
        {
            IngesterArray dataArr = accumulator_.getData(fieldIdx * mnemonicSet_.getMaxColumn(),
                                                         mnemonicSet_.getChannels());

            dataMap.insert({fieldName, dataArr});
            fieldIdx++;
        }

        accumulator_.reset();

        return dataMap;
    }
}  // namespace Ingester

