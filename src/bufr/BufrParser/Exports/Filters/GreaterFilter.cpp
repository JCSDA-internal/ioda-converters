/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "GreaterFilter.h"

#include <ostream>

#include "eckit/exception/Exceptions.h"

#include "../RowSlice.h"


namespace Ingester
{
    GreaterFilter::GreaterFilter(const std::string& mnemonic, float value) :
      mnemonic_(mnemonic),
      value_(value)
    {
    }

    void GreaterFilter::apply(BufrDataMap& dataMap)
    {
        std::vector<size_t> validRows;

        if (dataMap.find(mnemonic_) == dataMap.end())
        {
            std::ostringstream errStr;
            errStr << "Unknown mnemonic " << mnemonic_ << " found in greater than filter.";
            throw eckit::BadParameter(errStr.str());
        }

        const auto& array = dataMap.at(mnemonic_);

        for (size_t rowIdx = 0; rowIdx < static_cast<size_t>(array.rows()); rowIdx++)
        {
            if ((array.row(rowIdx) >= value_).all())
            {
                validRows.push_back(rowIdx);
            }
        }

        if (validRows.size() != static_cast<size_t>(array.rows()))
        {
            for (const auto& dataPair: dataMap)
            {
                dataMap[dataPair.first] = rowSlice(dataPair.second, validRows);
            }
        }
    }
}  // namespace Ingester
