/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <ostream>
#include <vector>

#include "BufrMnemonicSet.h"
#include "BufrTypes.h"

namespace Ingester
{
    BufrMnemonicSet::BufrMnemonicSet(const std::vector<std::string>& mnemonics, const Channels& channels) :
        mnemonics_(mnemonics),
        mnemonicsStr_(makeMnemonicsStr(mnemonics)),
        channels_(channels),
        maxColumn_(findMaxChannel(channels))
    {
    }

    std::string BufrMnemonicSet::makeMnemonicsStr(std::vector<std::string> mnemonics)
    {
        std::ostringstream mnemonicsStrStream;
        for (auto mnemonicsIt = mnemonics.begin();
             mnemonicsIt < mnemonics.end();
             mnemonicsIt++)
        {
            mnemonicsStrStream << *mnemonicsIt;

            if (mnemonicsIt != mnemonics.end() - 1)
            {
                mnemonicsStrStream << " ";
            }
        }

        return mnemonicsStrStream.str();
    }

    size_t BufrMnemonicSet::findMaxChannel(const Channels& channels)
    {
        size_t maxChannel = 0;
        for (auto channel : channels)
        {
            if (channel > static_cast<int> (maxChannel))
            {
                maxChannel = channel;
            }
        }

        return maxChannel;
    }

}  // namespace Ingester
