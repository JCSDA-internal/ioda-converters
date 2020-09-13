/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BufrParser.h"

#include <iostream>
#include <map>

#include "bufr.interface.h"

#include "BufrParser/BufrCollectors/BufrCollectors.h"
#include "BufrMnemonicSet.h"
#include "IngesterData.h"

#include "Exports/MnemonicExport.h"
#include "Exports/DatetimeExport.h"
#include "Exports/Export.h"


namespace Ingester
{
    static const unsigned int SUBSET_STR_LEN = 25;

    BufrParser::BufrParser(BufrDescription &description) :
        description_(description),
        fileUnit_(0)
    {
        reset();
    }

    BufrParser::~BufrParser()
    {
        closeBufrFile();
    }

    std::shared_ptr <IngesterData> BufrParser::parse(const size_t maxMsgsToParse)
    {
        assert(fileUnit_ > 0);

        auto collectors = BufrCollectors(fileUnit_);
        collectors.addMnemonicSets(description_.getMnemonicSets());

        char subset[SUBSET_STR_LEN];
        int iddate;

        unsigned int messageNum = 0;
        while (ireadmg_f(fileUnit_, subset, &iddate, SUBSET_STR_LEN) == 0)
        {
            while (ireadsb_f(fileUnit_) == 0)
            {
                collectors.collect();
            }

            if (maxMsgsToParse > 0 && ++messageNum >= maxMsgsToParse) break;
        }

        return exportData(collectors.finalize());
    }

    std::shared_ptr<IngesterData> BufrParser::exportData(const BufrDataMap& srcData)
    {
        auto outputData = std::make_shared<IngesterData>();
        auto exportMap = description_.getExportMap();

        unsigned int size = 0;
        auto exportIt = exportMap.begin();
        while (exportIt != exportMap.end())
        {
            auto key = exportIt->first;
            auto data_exporter = exportIt->second;

            size = (size == 0) ? srcData.begin()->second.rows() : size;

            if (auto ex = std::dynamic_pointer_cast<MnemonicExport>(data_exporter))
            {
                outputData->add<IngesterArray>(key, ex->exportData(srcData));
            }
            else if (auto ex = std::dynamic_pointer_cast<DatetimeExport>(data_exporter))
            {
                outputData->add<IngesterStrVector>(key, ex->exportData(srcData));
            }
            else
            {
                std::cout << "WARNING: BufrParser::exportStrs: Could not find mnemonic "
                          << key
                          << " in src data."
                          << std::endl;
            }

            exportIt++;
        }

        outputData->setSize(size);

        return outputData;
    }

    void BufrParser::openBufrFile(const std::string &filepath)
    {
        fileUnit_ = 11;
        open_f(fileUnit_, filepath.c_str());
        openbf_f(fileUnit_, "IN", fileUnit_);
    }

    void BufrParser::closeBufrFile()
    {
        closbf_f(fileUnit_);
        close_f(fileUnit_);
        fileUnit_ = 0;
    }

    void BufrParser::reset()
    {
        if (fileUnit_ != 0)
        {
            closeBufrFile();
        }

        openBufrFile(description_.filepath());
    }
}  // namespace Ingester
