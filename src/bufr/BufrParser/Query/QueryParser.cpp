/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "QueryParser.h"

#include <iostream>
#include <algorithm>

#include "eckit/exception/Exceptions.h"

namespace Ingester {
namespace bufr {
    std::vector<Query> QueryParser::parse(const std::string& queryStr)
    {
        std::vector<Query> queries;
        for (auto& subStr : QueryParser::splitMultiquery(queryStr))
        {
            queries.emplace_back(QueryParser::splitQueryStr(subStr));
        }

        return queries;
    }

    std::vector<std::string> QueryParser::splitMultiquery(const std::string &query)
    {
        std::vector<std::string> subqueries;

        // Remove whitespace from query and assign to working_str
        std::string working_str = query;
        working_str.erase(
                std::remove(working_str.begin(), working_str.end(), ' '), working_str.end());

        if (working_str.substr(0, 1) == "[") {
            if (working_str.substr(working_str.length() - 1, 1) != "]") {
                std::stringstream errMsg;
                errMsg << "Query Parser: multi query is lacking closing brackets." << std::endl;
                throw eckit::BadParameter(errMsg.str());
            }

            working_str = working_str.substr(1, working_str.length() - 2);

            std::vector<size_t> comma_positions;

            size_t last_pos = 0;
            while (working_str.find(',', last_pos) != std::string::npos)
            {
                auto comma_idx = working_str.find(',', last_pos);
                comma_positions.push_back(comma_idx);
                last_pos = comma_idx + 1;
            }

            last_pos = 0;
            subqueries.reserve(comma_positions.size() + 1);
            for (size_t commaIdx = 0; commaIdx < comma_positions.size() + 1; ++commaIdx)
            {
                if (commaIdx < comma_positions.size())
                {
                    subqueries.push_back(
                            working_str.substr(last_pos, comma_positions[commaIdx] - last_pos));
                    last_pos = comma_positions[commaIdx] + 1;
                }
                else
                {
                    subqueries.push_back(
                            working_str.substr(last_pos, working_str.length() - last_pos));
                }
            }
        }
        else
        {
            subqueries.push_back(working_str);
        }

        return subqueries;
    }

    Query QueryParser::splitQueryStr(const std::string& query)
    {
        // Find positions of slashes
        std::vector<size_t> slashPositions;
        size_t slashIdx = 0;
        size_t charIdx = 0;
        for (charIdx = 0; charIdx < query.length(); ++charIdx) {
            if (charIdx > 0 && query[charIdx] == '/') {
                slashPositions.push_back(charIdx);
                slashIdx++;
            }
        }

        if (slashPositions.size() < 1) {
            std::stringstream errMsg;
            errMsg << "Query Parser: Not a valid query string." << std::endl;
            throw eckit::BadParameter(errMsg.str());
        }

        // Capture the subset string
        auto subset = query.substr(0, slashPositions[0]);

        std::vector<std::string> mnemonicStrings(slashPositions.size());

        // Capture the sequence mnemonic strings
        for (size_t mnemonicIdx = 0; mnemonicIdx < mnemonicStrings.size() - 1; ++mnemonicIdx)
        {
            mnemonicStrings[mnemonicIdx] =
                    query.substr(slashPositions[mnemonicIdx] + 1,
                                 slashPositions[mnemonicIdx + 1] - slashPositions[mnemonicIdx] - 1);
        }

        // Get the last element
        std::string lastElement = query.substr(slashPositions[slashPositions.size() - 1] + 1);

        // Parse last element
        int index = -1;
        size_t startSubscript = lastElement.find_first_of("[");
        size_t endSubscript = lastElement.find_first_of("]");
        if (startSubscript != std::string::npos && endSubscript != std::string::npos)
        {
            index = std::stoi(lastElement.substr(startSubscript + 1,
                                                  endSubscript - startSubscript - 1));
            mnemonicStrings[mnemonicStrings.size() - 1] = lastElement.substr(0, startSubscript);
        }
        else
        {
            if (endSubscript != std::string::npos || startSubscript != std::string::npos)
            {
                std::stringstream errMsg;
                errMsg << "Query Parser: Not a valid query string. Extra brackets." << std::endl;
                throw eckit::BadParameter(errMsg.str());
            }

            mnemonicStrings.back() = lastElement;
        }

        auto queryObj = Query();
        queryObj.queryStr = query;
        queryObj.subset = subset;
        queryObj.mnemonics = mnemonicStrings;
        queryObj.index = index;

        return queryObj;
    }
}  // namespace bufr
}  // namespace Ingester
