/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BufrParser.h"

#include <bufr.interface.h>
#include <map>

#include "BufrParser/BufrCollectors/BufrCollectors.h"
#include "BufrMnemonicSet.h"
#include "IngesterData.h"


namespace Ingester
{
    static const unsigned int SUBSET_STR_LEN = 25;

    BufrParser::BufrParser(BufrDescription &description) :
        description_(description),
        fileUnit_(0)
    {
        fileUnit_ = openBufrFile(description_.filepath());
    }

    BufrParser::~BufrParser()
    {
        closeBufrFile(fileUnit_);
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

        return collectors.finalize();
    }

    int BufrParser::openBufrFile(const std::string &filepath)
    {
        static const int fileUnit = 11;

        open_f(fileUnit, filepath.c_str());
        openbf_f(fileUnit, "IN", fileUnit);

        return fileUnit;
    }

    void BufrParser::closeBufrFile(const int fileUnit)
    {
        closbf_f(fileUnit);
        close_f(fileUnit);
    }
}  // namespace Ingester
