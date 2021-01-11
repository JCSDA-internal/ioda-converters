/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BoundingFilter.h"

#include <ostream>

#include "eckit/exception/Exceptions.h"

#include "../RowSlice.h"

namespace Ingester
{
    BoundingFilter::BoundingFilter(const std::string& mnemonic,
                   std::shared_ptr<float> lowerBound,
                   std::shared_ptr<float> upperBound) :
      mnemonic_(mnemonic),
      lowerBound_(lowerBound),
      upperBound_(upperBound)
    {
        if (!upperBound && !lowerBound)
        {
            std::stringstream errStr;
            errStr << "BoundingFilter must contain either an upper or lower bound or both.";
            throw eckit::BadParameter(errStr.str());
        }

        if (upperBound && lowerBound && (*upperBound < *lowerBound))
        {
            std::stringstream errStr;
            errStr << "BoundingFilter upperBound must be greater or equal to lowerBound";
            throw eckit::BadParameter(errStr.str());
        }
    }

    void BoundingFilter::apply(BufrDataMap& dataMap)
    {
        std::vector<size_t> validRows;
        if (dataMap.find(mnemonic_) == dataMap.end())
        {
            std::ostringstream errStr;
            errStr << "Unknown mnemonic " << mnemonic_ << " found in bounding filter.";
            throw eckit::BadParameter(errStr.str());
        }

        const auto& array = dataMap.at(mnemonic_);
        for (size_t rowIdx = 0; rowIdx < static_cast<size_t>(array.rows()); rowIdx++)
        {
            if (lowerBound_ && upperBound_)
            {
                if ((array.row(rowIdx) >= *lowerBound_).all() &&
                    (array.row(rowIdx) <= *upperBound_).all())
                {
                    validRows.push_back(rowIdx);
                }
            }
            else
            {
                if ((lowerBound_ && (array.row(rowIdx) >= *lowerBound_).all()) ||
                    (upperBound_ && (array.row(rowIdx) <= *upperBound_).all()))
                {
                    validRows.push_back(rowIdx);
                }
            }
        }

        if (validRows.size() != static_cast<size_t>(array.rows()))
        {
            for (const auto& dataPair : dataMap)
            {
                dataMap[dataPair.first] = rowSlice(dataPair.second, validRows);
            }
        }
    }
}  // namespace Ingester
