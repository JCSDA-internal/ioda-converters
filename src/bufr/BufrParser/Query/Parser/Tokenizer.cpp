/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "Tokenizer.h"

namespace Ingester {
namespace bufr {
    std::vector<std::shared_ptr<Token>> Tokenizer::tokenize(const std::string &query)
    {
        std::vector<std::shared_ptr<Token>> tokens;
        std::string::const_iterator iter = query.begin();
        std::string::const_iterator end = query.end();
        while (iter != end) {
            // check if the next token matches a mnemonic
            if (MnemonicToken::match(iter, end)) {
                tokens.push_back(std::make_shared<MnemonicToken>(iter, end));
            } else if (SeperatorToken::match(iter, end)) {
                tokens.push_back(std::make_shared<SeperatorToken>(iter, end));
            } else if (IndexToken::match(iter, end)) {
                tokens.push_back(std::make_shared<IndexToken>(iter, end));
            } else if (FilterToken::match(iter, end)) {
                tokens.push_back(std::make_shared<FilterToken>(iter, end));
            } else {
                throw std::runtime_error("Tokenizer: Unknown token");
            }
        }
        return tokens;
    }
}  // bufr
}  // Ingester
