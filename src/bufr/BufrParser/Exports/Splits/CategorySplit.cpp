/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "CategorySplit.h"

#include <ostream>

#include "eckit/exception/Exceptions.h"


namespace Ingester
{
    CategorySplit::CategorySplit(const std::string& variable, const NameMap& nameMap) :
      nameMap_(nameMap),
      variable_(variable)
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

        const auto& dataObject = dataMap.at(variable_);

        for (const auto& mapPair : nameMap_)
        {
            // Find matching rows
            std::vector<size_t> indexVec;
            for (size_t rowIdx = 0; rowIdx < dataObject->nrows(); rowIdx++)
            {
                if (dataObject->getInt(rowIdx, 0) == mapPair.first)
                {
                    indexVec.push_back(rowIdx);
                }
            }

            // Make new data map
            BufrDataMap newDataMap;
            for (const auto& dataPair : dataMap)
            {
                const auto newArr = dataPair.second->slice(indexVec);
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
            const auto& dataObject = dataMap.at(variable_);
            for (auto rowIdx = 0; rowIdx < dataObject->nrows(); rowIdx++)
            {
                auto itemVal =  dataObject->getFloat(rowIdx, 0);
                if (trunc(itemVal) == itemVal)
                {
                    nameMap_.insert({static_cast<int> (itemVal),
                                     std::to_string(static_cast<int> (itemVal))});
                }
                else
                {
                    std::stringstream errStr;
                    errStr << "Can't turn " << variable_ << " into a category as it contains ";
                    errStr << "non-integer values.";
                    throw eckit::BadParameter(errStr.str());
                }
            }
        }

        if (nameMap_.empty())
        {
            std::stringstream errStr;
            errStr << "No categories could be identified for " << variable_ << ".";
            throw eckit::BadParameter(errStr.str());
        }
    }
}  // namespace Ingester
