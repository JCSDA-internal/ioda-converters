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

        std::vector<QueryData> getQueries(const SubsetVariant& variant) final;
        std::set<SubsetVariant> getSubsetVariants() const final;
    };
}  // namespace bufr
}  // namespace Ingester
