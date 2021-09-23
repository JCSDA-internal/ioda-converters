/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BufrParser.h"

#include <map>
#include <ostream>
#include <iostream>

#include "eckit/exception/Exceptions.h"

#include "DataContainer.h"
#include "DataObject/ArrayDataObject.h"
#include "DataObject/DataObject.h"
#include "DataObject/StrVecDataObject.h"
#include "Exports/Export.h"
#include "Exports/Splits/Split.h"

#include "File.h"
#include "ResultSet.h"
#include "QuerySet.h"

namespace Ingester
{
    BufrParser::BufrParser(const BufrDescription &description) :
        description_(description),
        file_(bufr::File(description_.filepath(),
                         description_.isWmoFormat(),
                         description_.tablepath()))
    {
    }

    BufrParser::BufrParser(const eckit::Configuration& conf) :
        description_(BufrDescription(conf)),
        file_(bufr::File(description_.filepath(),
                         description_.isWmoFormat(),
                         description_.tablepath()))
    {
    }

    BufrParser::~BufrParser()
    {
        file_.close();
    }

    std::shared_ptr <DataContainer> BufrParser::parse(const std::size_t maxMsgsToParse)
    {
        std::cout << "Start" << std::endl;

        auto querySet = bufr::QuerySet();

        for (const auto& var : description_.getExport().getVariables())
        {
            for (const auto& queryPair : var->getQueryList())
            {
                querySet.add(queryPair.query, queryPair.name);
            }
        }

        std::cout << "Built Query Parser" << std::endl;

        auto result_set = file_.execute(querySet, maxMsgsToParse);

        std::cout << "Executed Queries" << std::endl;

        auto srcData = BufrDataMap();

        for (const auto& var : description_.getExport().getVariables())
        {
            for (const auto& queryInfo : var->getQueryList())
            {
                auto result = result_set.get(queryInfo.name, queryInfo.groupByField);

                if (auto floatRes = std::dynamic_pointer_cast<bufr::Result<float>>(result))
                {
                    auto dataArr = Eigen::Map<IngesterArray>(floatRes->data.data(),
                                                             floatRes->dims[0],
                                                             floatRes->dims[1]);

                    srcData[queryInfo.name] = std::make_shared<ArrayDataObject>(dataArr);
                }
                else if (auto strRes = std::dynamic_pointer_cast<bufr::Result<std::string>>(result))
                {
                    srcData[queryInfo.name] = std::make_shared<StrVecDataObject>(strRes->data);
                }
            }
        }

        std::cout << "Built Bufr Data" << std::endl;

        return exportData(srcData);
    }

    std::shared_ptr<DataContainer> BufrParser::exportData(const BufrDataMap& srcData)
    {
        auto exportDescription = description_.getExport();

        auto filters = exportDescription.getFilters();
        auto splits = exportDescription.getSplits();
        auto vars = exportDescription.getVariables();

        // Filter
        BufrDataMap dataCopy = srcData;  // make mutable copy
        for (const auto& filter : filters)
        {
            filter->apply(dataCopy);
        }

        // Split
        CategoryMap catMap;
        for (const auto& split : splits)
        {
            std::ostringstream catName;
            catName << "splits/" << split->getName();
            catMap.insert({catName.str(), split->subCategories(dataCopy)});
        }

        BufrParser::CatDataMap splitDataMaps;
        splitDataMaps.insert({std::vector<std::string>(), dataCopy});
        for (const auto& split : splits)
        {
            splitDataMaps = splitData(splitDataMaps, *split);
        }

        // Export
        auto exportData = std::make_shared<DataContainer>(catMap);
        for (const auto& dataPair : splitDataMaps)
        {
            for (const auto& var : vars)
            {
                std::ostringstream pathStr;
                pathStr << "variables/" << var->getExportName();

                exportData->add(pathStr.str(),
                                var->exportData(dataPair.second),
                                dataPair.first);
            }
        }

        return exportData;
    }

    BufrParser::CatDataMap BufrParser::splitData(BufrParser::CatDataMap& splitMaps, Split& split)
    {
        CatDataMap splitDataMap;

        for (const auto& splitMapPair : splitMaps)
        {
            auto newData = split.split(splitMapPair.second);

            for (const auto& newDataPair : newData)
            {
                auto catVect = splitMapPair.first;
                catVect.push_back(newDataPair.first);
                splitDataMap.insert({catVect, newDataPair.second});
            }
        }

        return splitDataMap;
    }

    void BufrParser::reset()
    {
        file_.rewind();
    }

    void BufrParser::printMap(const BufrParser::CatDataMap& map)
    {
        for (const auto &mp : map)
        {
            std::cout << " keys: ";
            for (const auto &s : mp.first)
            {
                std::cout << s;
            }

            std::cout << " subkeys: ";
            for (const auto &m2p : mp.second)
            {
                std::cout << m2p.first << " " << m2p.second->nrows() << " ";
            }

            std::cout << std::endl;
        }
    }
}  // namespace Ingester
