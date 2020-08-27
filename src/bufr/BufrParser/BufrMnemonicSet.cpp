/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>
#include <iterator>
#include <vector>

#include "BufrMnemonicSet.h"
#include "BufrTypes.h"

namespace Ingester
{
    BufrMnemonicSet::BufrMnemonicSet(const std::string& nmemonicsStr, const Channels &channels) :
        mnemonicsStr_(nmemonicsStr),
        mnemonics_(tokenizeMnemonics(nmemonicsStr)),
        channels_(channels)
    {
        maxColumn_ = 0;
        for (auto col : channels)
        {
            if (col > static_cast<int> (maxColumn_))
            {
                maxColumn_ = col;
            }
        }
    }

    std::vector<std::string> BufrMnemonicSet::tokenizeMnemonics(const std::string& nmemonicsStr)
    {
        // Tokenize the string into individual mnemonic strings
        std::istringstream buf(nmemonicsStr);
        std::istream_iterator <std::string> beg(buf), end;
        std::vector <std::string> tokens(beg, end);
        return tokens;
    }
}  // namespace Ingester
