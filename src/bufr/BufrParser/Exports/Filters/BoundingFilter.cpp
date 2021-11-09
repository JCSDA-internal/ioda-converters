/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BoundingFilter.h"

#include <ostream>

#include "eckit/exception/Exceptions.h"

#include "DataObject.h"

#include "Eigen/Dense"


namespace Ingester
{
    typedef Eigen::Array<float, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> EigArray;

    BoundingFilter::BoundingFilter(const std::string& variable,
                   std::shared_ptr<float> lowerBound,
                   std::shared_ptr<float> upperBound) :
      variable_(variable),
      lowerBound_(lowerBound),
      upperBound_(upperBound)
    {
        if (!upperBound && !lowerBound)
        {
            std::stringstream errStr;
            errStr << "BoundingFilter must contain either upperBound, lowerBound or both.";
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
        if (dataMap.find(variable_) == dataMap.end())
        {
            std::ostringstream errStr;
            errStr << "Unknown variable " << variable_ << " found in bounding filter.";
            throw eckit::BadParameter(errStr.str());
        }

        if (const auto& var = std::dynamic_pointer_cast<DataObject<float>>(dataMap.at(variable_)))
        {
            auto dims = var->getDims();
            size_t extraDims = 1;

            for (size_t dimIdx = 1; dimIdx < dims.size(); ++dimIdx)
            {
                extraDims *= dims[dimIdx];
            }

            auto rawData = var->getRawData(); // Make local copy otherwise you get weird memory corruption issue.
            auto array = Eigen::Map<EigArray> (rawData.data(), dims[0], extraDims);

            for (std::size_t rowIdx = 0; rowIdx < dims[0]; rowIdx++)
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

            if (validRows.size() != static_cast<size_t>(dims[0]))
            {
                for (const auto& dataPair : dataMap)
                {
                    dataMap[dataPair.first] = dataPair.second->slice(validRows);
                }
            }
        }
        else
        {
            std::stringstream errStr;
            errStr << "BoundingFilter variable must be a array of numbers (found list of strings).";
            throw eckit::BadParameter(errStr.str());
        }
    }
}  // namespace Ingester
