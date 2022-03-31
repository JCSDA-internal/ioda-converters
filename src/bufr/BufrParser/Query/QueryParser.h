/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

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
