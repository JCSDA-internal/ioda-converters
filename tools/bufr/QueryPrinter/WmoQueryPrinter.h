//
// Created by Ronald McLaren on 10/19/22.
//

#pragma once

#include "QueryPrinter.h"


namespace Ingester {
namespace bufr {

    class QueryData;

    class WmoQueryPrinter : public QueryPrinter
    {
     public:
       WmoQueryPrinter(const std::string& filepath, const std::string& tablepath);
       ~WmoQueryPrinter() = default;

        std::vector<QueryData> getQueries(const SubsetVariant& variant) final;
        std::set<SubsetVariant> getSubsetVariants() const final;

     private:
        const int FileUnitTable1 = 13;
        const int FileUnitTable2 = 14;
    };
}  // namespace bufr
}  // namespace Ingester
