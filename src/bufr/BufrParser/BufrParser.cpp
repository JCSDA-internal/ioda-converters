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

#include "bufr.interface.h"
#include "BufrParser/BufrCollectors/BufrCollectors.h"
#include "BufrMnemonicSet.h"
#include "DataContainer.h"
#include "Exports/Export.h"
#include "Exports/Splits/Split.h"


namespace Ingester
{
    BufrParser::BufrParser(const BufrDescription &description) :
        description_(description),
        fortranFileId_(0),
        table1FileId_(0),
        table2FileId_(0)
    {
        reset();
    }

    BufrParser::BufrParser(const eckit::Configuration& conf) :
        description_(BufrDescription(conf)),
        fortranFileId_(0),
        table1FileId_(0),
        table2FileId_(0)
    {
        reset();
    }

    BufrParser::~BufrParser()
    {
        closeBufrFile();
    }

    std::shared_ptr <DataContainer> BufrParser::parse(const size_t maxMsgsToParse)
    {
        const unsigned int SubsetStringLength = 25;

        if (fortranFileId_ <= 10)
        {
            throw eckit::BadValue("Fortran File ID is an invalid number (must be > 10).");
        }

        auto collectors = BufrCollectors(fortranFileId_);
        collectors.addMnemonicSets(description_.getMnemonicSets());

        char subset[SubsetStringLength];
        int iddate;

        unsigned int messageNum = 0;
        while (ireadmg_f(fortranFileId_, subset, &iddate, SubsetStringLength) == 0)
        {
            while (ireadsb_f(fortranFileId_) == 0)
            {
                collectors.collect();
            }

            if (maxMsgsToParse > 0 && ++messageNum >= maxMsgsToParse) break;
        }

        return exportData(collectors.finalize());
    }

    std::shared_ptr<DataContainer> BufrParser::exportData(const BufrDataMap& srcData)
    {
        auto exportDescription = description_.getExport();

        auto filters = exportDescription.getFilters();
        auto splitMap = exportDescription.getSplits();
        auto varMap = exportDescription.getVariables();

        // Filter
        BufrDataMap dataCopy = srcData;  // make mutable copy
        for (const auto& filter : filters)
        {
            filter->apply(dataCopy);
        }

        // Split
        CategoryMap catMap;
        for (const auto& splitPair : splitMap)
        {
            std::ostringstream catName;
            catName << "splits/" << splitPair.first;
            catMap.insert({catName.str(), splitPair.second->subCategories(dataCopy)});
        }

        BufrParser::CatDataMap splitDataMaps;
        splitDataMaps.insert({std::vector<std::string>(), dataCopy});
        for (const auto& splitPair : splitMap)
        {
            splitDataMaps = splitData(splitDataMaps, *splitPair.second);
        }

        // Export
        auto exportData = std::make_shared<DataContainer>(catMap);
        for (const auto& dataPair : splitDataMaps)
        {
            for (const auto& varPair : varMap)
            {
                std::ostringstream pathStr;
                pathStr << "variables/" << varPair.first;

                exportData->add(pathStr.str(),
                                varPair.second->exportData(dataPair.second),
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

    void BufrParser::openBufrFile(const std::string& filepath,
                                  bool isStdFormat,
                                  const std::string& tablepath)
    {
        fortranFileId_ = 11;  // Fortran file id must be a integer > 10
        open_f(fortranFileId_, filepath.c_str());

        if (!isStdFormat)
        {
            openbf_f(fortranFileId_, "IN", fortranFileId_);
        }
        else
        {
            openbf_f(fortranFileId_, "SEC3", fortranFileId_);

            if (!tablepath.empty())  // else use the default tables
            {
                table1FileId_ = fortranFileId_ + 1;
                table2FileId_ = fortranFileId_ + 2;
                mtinfo_f(tablepath.c_str(), table1FileId_, table2FileId_);
            }
        }
    }

    void BufrParser::closeBufrFile()
    {
        exitbufr_f();

        fortranFileId_ = 0;
        table1FileId_ = 0;
        table2FileId_ = 0;
    }

    void BufrParser::reset()
    {
        if (fortranFileId_ != 0)
        {
            closeBufrFile();
        }

        openBufrFile(description_.filepath(),
                     description_.isStdFormat(),
                     description_.tablepath());
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
                std::cout << m2p.first << " " << m2p.second.rows() << " ";
            }

            std::cout << std::endl;
        }
    }
}  // namespace Ingester
