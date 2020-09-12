/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <bufr.interface.h>

#include "BufrRepCollector.h"


namespace Ingester
{

    BufrRepCollector::BufrRepCollector(const int fileUnit, const BufrMnemonicSet mnemonicSet) :
        BufrCollector(fileUnit,
                      BufrAccumulator(mnemonicSet.getSize() * mnemonicSet.getMaxColumn())),
        mnemonicSet_(mnemonicSet)
    {
        scratchData_ = new double[mnemonicSet.getSize() * mnemonicSet.getMaxColumn()];
    }

    BufrRepCollector::~BufrRepCollector()
    {
        delete[] scratchData_;
    }

    void BufrRepCollector::collect()
    {
        int result;

        ufbrep_f(fileUnit_,
                 reinterpret_cast<void**>(&scratchData_),
                 mnemonicSet_.getSize(),
                 mnemonicSet_.getMaxColumn(),
                 &result,
                 mnemonicSet_.getMnemonicsStr().c_str());

        accumulator_.addRow(scratchData_);
    }

    BufrDataMap BufrRepCollector::finalize()
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
