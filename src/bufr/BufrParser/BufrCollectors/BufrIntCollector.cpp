/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "bufr.interface.h"

#include "BufrIntCollector.h"


namespace Ingester
{
    BufrIntCollector::BufrIntCollector(const int fortranFileId, const BufrMnemonicSet& mnemonicSet) :
        BufrCollector(fortranFileId, mnemonicSet)
    {
        scratchData_.resize(accumulator_.getNumColumns());
    }

    void BufrIntCollector::collect()
    {
        double* scratchDataPtr = scratchData_.data();

        int result;
        ufbint_f(fortranFileId_,
                 reinterpret_cast<void**> (&scratchDataPtr),
                 mnemonicSet_.getSize(),
                 mnemonicSet_.getMaxColumn(),
                 &result,
                 mnemonicSet_.getMnemonicsStr().c_str());

        accumulator_.addRow(scratchData_);
    }
}  // namespace Ingester
