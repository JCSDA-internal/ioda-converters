/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "CategorySplit.h"

#include <ostream>

#include "eckit/exception/Exceptions.h"

namespace
{
    namespace ConfKeys
    {
        const char* NameMap = "map";
        const char* Variable = "variable";
    }  // namespace ConfKeys
}  // namespace

namespace Ingester
{
    CategorySplit::CategorySplit(const std::string& name, const eckit::LocalConfiguration& conf) :
        Split(name, conf),
        variable_(conf.getString(ConfKeys::Variable))
    {
        if (conf.has(ConfKeys::NameMap))
        {
            const auto& mapConf = conf.getSubConfiguration(ConfKeys::NameMap);
            for (const std::string& mapKey : mapConf.keys())
            {
                auto intKey = mapKey.substr(1, mapKey.size());
                nameMap_.insert({std::stoi(intKey), mapConf.getString(mapKey)});
            }
        }
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

    std::unordered_map<std::string, BufrDataMap> CategorySplit::split(const BufrDataMap &dataMap)
    {
        updateNameMap(dataMap);

        std::unordered_map<std::string, BufrDataMap> dataMaps;

        const auto& dataObject = dataMap.at(variable_);

        for (const auto& mapPair : nameMap_)
        {
            // Find matching rows
            std::vector<size_t> indexVec;
            for (auto rowIdx = 0; rowIdx < dataObject->getDims()[0]; rowIdx++)
            {
                auto location = Location(dataObject->getDims().size(), 0);
                location[0] = rowIdx;

                if (dataObject->getAsInt(location) == mapPair.first)
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
            for (auto rowIdx = 0; rowIdx < dataObject->getDims()[0]; rowIdx++)
            {
                auto location = Location(dataObject->getDims().size(), 0);
                location[0] = rowIdx;

                if (auto dat = std::dynamic_pointer_cast<DataObject<int>> (dataObject))
                {
                    auto itemVal = dat->get(location);
                    nameMap_.insert({itemVal, std::to_string(itemVal)});
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
