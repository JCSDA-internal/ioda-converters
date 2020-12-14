/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "CategorySplit.h"

#include <ostream>

#include "eckit/exception/Exceptions.h"

#include "../RowSlice.h"


namespace Ingester
{
    CategorySplit::CategorySplit(const std::string& mnemonic, const NameMap& nameMap) :
      nameMap_(nameMap),
      mnemonic_(mnemonic)
    {
    }

    std::vector<std::string> CategorySplit::subCategories()
    {
        std::vector<std::string> categories;
        for (const auto& name : nameMap_)
        {
            categories.push_back(name.second);
        }

        return categories;
    }

    std::map<std::string, BufrDataMap> CategorySplit::split(const BufrDataMap &dataMap)
    {
        std::map<std::string, BufrDataMap> dataMaps;

        const IngesterArray& mnemonicArr = dataMap.at(mnemonic_);

        for (const auto& mapPair : nameMap_)
        {
            // Find matching rows
            std::vector<size_t> indexVec;
            for (int rowIdx = 0;
                 rowIdx < static_cast<int>(dataMap.at(mnemonic_).rows());
                 rowIdx++)
            {
                if (abs(mnemonicArr.row(rowIdx)[0] - static_cast<float>(mapPair.first)) < .0001)
                {
                    indexVec.push_back(rowIdx);
                }
            }

            // Make new data map
            BufrDataMap newDataMap;
            for (const auto& dataPair : dataMap)
            {
                const auto newArr = rowSlice(dataPair.second, indexVec);
                newDataMap.insert({dataPair.first, newArr});
            }

            dataMaps.insert({mapPair.second, newDataMap});
        }

        return dataMaps;
    }
}  // namespace Ingester
