//
// Created by rmclaren on 1/28/22.
//

#pragma once

#include <string>
#include <vector>

namespace Ingester {
namespace bufr {
    class QueryParser
    {
    public:

        static std::vector<std::string> splitMultiquery(const std::string& query);
        static void splitQueryStr(const std::string& query,
                                  std::string& subset,
                                  std::vector<std::string>& mnemonics,
                                  int& index);

    private:
        QueryParser();
    };
}  // namespace bufr
}  // namespace Ingester
