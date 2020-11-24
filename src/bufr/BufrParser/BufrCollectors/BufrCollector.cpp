/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BufrCollector.h"


namespace BufrParser
{
    BufrCollector::BufrCollector(const int fortranFileId, const BufrMnemonicSet mnemonicSet) :
        fortranFileId_(fortranFileId),
        accumulator_(BufrAccumulator(mnemonicSet.getSize() * mnemonicSet.getMaxColumn())),
        mnemonicSet_(mnemonicSet)
    {
    }

    BufrDataMap BufrCollector::finalize()
    {
        IodaEncoder::EncoderArrayMap dataMap;
        size_t fieldIdx = 0;
        for (const auto &fieldName : mnemonicSet_.getMnemonics())
        {
            IodaEncoder::EncoderArray dataArr =
                accumulator_.getData(fieldIdx * mnemonicSet_.getMaxColumn(),
                                     mnemonicSet_.getChannels());

            dataMap.insert({fieldName, dataArr});
            fieldIdx++;
        }

        accumulator_.reset();

        return dataMap;
    }
}  // namespace BufrParser

