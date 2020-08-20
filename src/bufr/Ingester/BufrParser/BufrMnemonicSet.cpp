/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>
#include <vector>
#include <sstream>
#include <iterator>

#include "BufrMnemonicSet.h"
#include "BufrTypes.h"

using namespace Ingester;
using namespace std;

BufrMnemonicSet::BufrMnemonicSet(const string& nmemonicsStr, const Channels& channels) :
    mnemonicsStr_(nmemonicsStr),
    mnemonics_(tokenizeMnemonics(nmemonicsStr)),
    channels_(channels)
{
    maxColumn_ = 0;
    for (auto col : channels)
    {
        if (col > (int) maxColumn_)
        {
            maxColumn_ = col;
        }
    }
}

vector<string> BufrMnemonicSet::tokenizeMnemonics(const string& nmemonicsStr)
{
    //Tokenize the string into individual mnemonic strings
    istringstream buf(nmemonicsStr);
    istream_iterator<string> beg(buf), end;
    vector<string> tokens(beg, end);
    return tokens;
}
