/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "bufr.interface.h"

#include "BufrRepCollector.h"


namespace BufrParser
{
    BufrRepCollector::BufrRepCollector(const int fortranFileId, const BufrMnemonicSet& mnemonicSet):
        BufrCollector(fortranFileId, mnemonicSet)
    {
        scratchData_.resize(accumulator_.getNumColumns());
        floatTypeScratchData_.resize(accumulator_.getNumColumns());
    }

    void BufrRepCollector::collect()
    {
        double* scratchDataPtr = scratchData_.data();

        int result;
        ufbrep_f(fortranFileId_,
                 reinterpret_cast<void**>(&scratchDataPtr),
                 mnemonicSet_.getSize(),
                 mnemonicSet_.getMaxColumn(),
                 &result,
                 mnemonicSet_.getMnemonicsStr().c_str());

        for (size_t colIdx = 0; colIdx < accumulator_.getNumColumns(); colIdx++)
        {
            floatTypeScratchData_[colIdx] =
                static_cast<IodaEncoder::FloatType>(scratchData_[colIdx]);
        }

        accumulator_.addRow(floatTypeScratchData_);
    }
}  // namespace BufrParser
