/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#if __has_include("bufr_interface.h")  // TODO(rmclaren): Remove this in future
    #include "bufr_interface.h"
#else
    #include "bufr.interface.h"
#endif

#include "BufrIntCollector.h"


namespace Ingester
{
    BufrIntCollector::BufrIntCollector(const int fortranFileId, const BufrMnemonicSet& mnemonicSet):
        BufrCollector(fortranFileId, mnemonicSet)
    {
        scratchData_.resize(accumulator_.getNumColumns());
        floatTypeScratchData_.resize(accumulator_.getNumColumns());
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

        for (Eigen::Index colIdx = 0; colIdx < accumulator_.getNumColumns(); colIdx++)
        {
            floatTypeScratchData_[colIdx] = static_cast<FloatType>(scratchData_[colIdx]);
        }

        accumulator_.addRow(floatTypeScratchData_);
    }
}  // namespace Ingester
