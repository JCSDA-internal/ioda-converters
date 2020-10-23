/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <ostream>
#include <vector>

#include "eckit/exception/Exceptions.h"

#include "BufrMnemonicSet.h"
#include "BufrTypes.h"

namespace Ingester
{
    BufrMnemonicSet::BufrMnemonicSet(const std::vector<std::string>& mnemonics,
                                     const Channels& channels) :
        mnemonics_(mnemonics),
        mnemonicsStr_(makeMnemonicsStr(mnemonics)),
        channels_(channels),
        maxColumn_(*std::max_element(channels.begin(), channels.end()))
    {
        if (std::find_if(channels.begin(), channels.end(), [](const auto x){ return x < 1; }) \
            != channels.end())
        {
            throw eckit::BadParameter("All channel numbers must be >= 1.");
        }
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
}  // namespace Ingester
