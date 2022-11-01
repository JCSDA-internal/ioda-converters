/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "NcepQueryPrinter.h"

#include <memory>
#include <iostream>

#include "QueryPrinter.h"
#include "../../../src/bufr/BufrParser/Query/DataProvider/NcepDataProvider.h"
#include "../../../src/bufr/BufrParser/Query/SubsetTable.h"

namespace Ingester {
namespace bufr {

    NcepQueryPrinter::NcepQueryPrinter(const std::string& filepath) :
      QueryPrinter(std::make_shared<NcepDataProvider>(filepath))
    {
    }

    std::vector<QueryData> NcepQueryPrinter::getQueries(const SubsetVariant& variant)
    {
        bool finished = false;

        std::vector<QueryData> queryData;

        auto& dataProvider = dataProvider_;
        auto processSubset = [&queryData, &finished, &dataProvider]() mutable
        {
            queryData = SubsetTable(dataProvider).allQueryData();
            finished = true;
        };


        auto continueProcessing = [&finished]() -> bool
        {
            return !finished;
        };

        dataProvider_->run(QuerySet({variant.subset}),
                           processSubset,
                          [](){},
                           continueProcessing);

        return queryData;
    }

//    std::set<std::string> DataProvider::getSubsets()
//    {
//        static const int SubsetLen = 9;
//        int iddate;
//
//        std::set<std::string> subsets;
//
//        char subset[SubsetLen];
//        while (ireadmg_f(FileUnit, subset, &iddate, SubsetLen) == 0)
//        {
//            auto str_subset = std::string(subset);
//            str_subset.erase(
//                remove_if(str_subset.begin(), str_subset.end(), isspace), str_subset.end());
//            subsets.insert(str_subset);
//        }
//
//        return subsets;
//    }

    std::set<SubsetVariant> NcepQueryPrinter::getSubsetVariants() const
    {
        std::set<SubsetVariant> subsets;

        auto& dataProvider = dataProvider_;
        auto processMsg = [&subsets, &dataProvider] () mutable
        {
            subsets.insert(dataProvider->getSubsetVariant());
        };

        dataProvider_->run(QuerySet({}),
                           [](){},
                           processMsg);

        return subsets;
    }
}  // namespace bufr
}  // namespace Ingester