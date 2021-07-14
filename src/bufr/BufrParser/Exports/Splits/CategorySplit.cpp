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
            query_(mnemonic)
    {
    }

    std::vector<std::string> CategorySplit::subCategories(const BufrDataMap& dataMap)
    {
        updateNameMap(dataMap);

        std::vector<std::string> categories;
        for (const auto& name : nameMap_)
        {
            categories.push_back(name.second);
        }

        return categories;
    }

    std::map<std::string, BufrDataMap> CategorySplit::split(const BufrDataMap &dataMap)
    {
        updateNameMap(dataMap);

        std::map<std::string, BufrDataMap> dataMaps;

        const IngesterArray& mnemonicArr = dataMap.at(query_);

        for (const auto& mapPair : nameMap_)
        {
            // Find matching rows
            std::vector<size_t> indexVec;
            for (int rowIdx = 0;
                 rowIdx < static_cast<int>(dataMap.at(query_).rows());
                 rowIdx++)
            {
                if (mnemonicArr.row(rowIdx)[0] == mapPair.first)
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

    void CategorySplit::updateNameMap(const BufrDataMap& dataMap)
    {
        if (nameMap_.empty())
        {
            auto& array = dataMap.at(query_);
            for (auto rowIdx = 0; rowIdx < array.rows(); rowIdx++)
            {
                auto itemVal =  array.row(rowIdx)[0];
                if (trunc(itemVal) == itemVal)
                {
                    nameMap_.insert({static_cast<int> (itemVal),
                                     std::to_string(static_cast<int> (itemVal))});
                }
                else
                {
                    std::stringstream errStr;
                    errStr << "Can't turn " << query_ << " into a category as it contains ";
                    errStr << "non-integer values.";
                    throw eckit::BadParameter(errStr.str());
                }
            }
        }

        if (nameMap_.empty())
        {
            std::stringstream errStr;
            errStr << "No categories could be identified for " << query_ << ".";
            throw eckit::BadParameter(errStr.str());
        }
    }
}  // namespace Ingester
