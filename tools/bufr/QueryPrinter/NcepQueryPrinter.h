//
// Created by Ronald McLaren on 10/19/22.
//

#pragma once

#include "QueryPrinter.h"

#include <string>
#include <vector>


namespace Ingester {
namespace bufr {

    class QueryData;

    class NcepQueryPrinter : public QueryPrinter
    {
     public:
        explicit NcepQueryPrinter(const std::string& filepath);

        std::vector<QueryData> getQueries(const std::string& subset) final;
        std::set<std::string> getSubsets() const final;
    };
}  // namespace bufr
}  // namespace Ingester
