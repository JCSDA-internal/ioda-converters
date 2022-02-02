//
// Created by rmclaren on 1/28/22.
//

#include "QueryParser.h"

#include <iostream>
#include <algorithm>

namespace Ingester {
namespace bufr {
     std::vector<std::string> QueryParser::splitMultiquery(const std::string &query) {
        std::vector<std::string> subqueries;

        // Remove whitespace from query and assign to working_str
        std::string working_str = query;
        working_str.erase(
                std::remove(working_str.begin(), working_str.end(), ' '), working_str.end());

        if (working_str.substr(0, 1) == "[") {
            if (working_str.substr(working_str.length() - 1, 1) != "]") {
                std::cerr << "Query Parser: multi query is lacking closing brackets." << std::endl;
                exit(1);
            }

            working_str = working_str.substr(1, working_str.length() - 2);

            std::vector<int> comma_positions;

            size_t comma_idx = 0;
            size_t last_pos = 0;
            while ((comma_idx = working_str.find(",", last_pos)) != std::string::npos)
            {
                comma_positions.push_back(comma_idx);
                last_pos = comma_idx + 1;
            }

            last_pos = 0;
            subqueries.reserve(comma_positions.size() + 1);
            for (size_t commaIdx = 0; commaIdx < comma_positions.size() + 1; ++commaIdx)
            {
                if (commaIdx < comma_positions.size())
                {
                    subqueries.push_back(working_str.substr(last_pos, comma_positions[commaIdx] - last_pos));
                }
                else
                {
                    subqueries.push_back(working_str.substr(last_pos, working_str.length() - last_pos));
                }

                last_pos = comma_positions[commaIdx] + 1;
            }
        }
        else
        {
            subqueries.push_back(working_str);
        }

        return subqueries;
    }


    void QueryParser::splitQueryStr(const std::string &query,
                                    std::string &subset,
                                    std::vector<std::string> &mnemonics,
                                    int &index) {
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
            std::cerr << "Query Parser: Not a valid query string." << std::endl;
            exit(1);
        }

        // Capture the subset string
        subset = query.substr(0, slashPositions[0]);

        // Capture the sequence mnemonic strings
        mnemonics.resize(slashPositions.size());
        for (size_t mnemonicIdx = 0; mnemonicIdx < mnemonics.size() - 1; ++mnemonicIdx) {
            mnemonics[mnemonicIdx] = query.substr(slashPositions[mnemonicIdx] + 1,
                                                  slashPositions[mnemonicIdx + 1] - slashPositions[mnemonicIdx] -
                                                  1);
        }

        // Get the last element
        std::string lastElement = query.substr(slashPositions[slashPositions.size() - 1] + 1);

        // Parse last element
        index = -1;
        size_t startSubscript = lastElement.find_first_of("[");
        size_t endSubscript = lastElement.find_first_of("]");
        if (startSubscript != std::string::npos && endSubscript != std::string::npos) {
            index = std::stoi(lastElement.substr(startSubscript + 1, endSubscript - startSubscript - 1));
            mnemonics[mnemonics.size()] = lastElement.substr(0, startSubscript);
        } else {
            if (endSubscript != std::string::npos || startSubscript != std::string::npos) {
                std::cerr << "Query Parser: Not a valid query string. Extra brackets." << std::endl;
                exit(1);
            }

            mnemonics.back() = lastElement;
        }
    }
}  // namespace bufr
}  // namespace Ingester
