//
// Created by Ronald McLaren on 10/19/22.
//

#include "WmoQueryPrinter.h"


#include <iostream>
#include <memory>
#include <unordered_map>

#include "bufr_interface.h"

#include "../../../src/bufr/BufrParser/Query/DataProvider/WmoDataProvider.h"
#include "../../../src/bufr/BufrParser/Query/SubsetTable.h"

namespace Ingester {
namespace bufr {
    WmoQueryPrinter::WmoQueryPrinter(const std::string& filepath, const std::string& tablepath) :
        QueryPrinter(std::make_shared<WmoDataProvider>(filepath, tablepath))
    {
    }

    std::vector<QueryData> WmoQueryPrinter::getQueries(const std::string& subset)
    {
        bool finished = false;

        std::cout << "!! "  << subset << std::endl;

        std::unordered_map<std::string, std::vector<QueryData>> dataMap;

        auto processMsg = []()
        {
        };

        auto& dataProvider = dataProvider_;
        auto processSubset = [&dataMap, &dataProvider]() mutable
        {
            std::string variant = dataProvider->getSubset();
            if (dataProvider->hasVariants())
            {
                variant = variant + "[" + std::to_string(dataProvider->variantId()) + "]";
            }

            std::cout << dataProvider->getSubset() <<" ## " << variant << std::endl;

            dataMap.insert({variant, SubsetTable(dataProvider).allQueryData()});
        };

        auto processFinish = []()
        {
        };

        dataProvider_->run(Ingester::bufr::QuerySet({subset}),
                           processMsg,
                           processSubset,
                           processFinish);

        std::vector<QueryData> queryData;
        for (const auto& queryObjs : dataMap)
        {
            queryData.insert(queryData.end(), queryObjs.second.begin(), queryObjs.second.end());
        }

        return queryData;
    }

    std::set<std::string> WmoQueryPrinter::getSubsets() const
    {
        std::set<std::string> subsets;

        auto& dataProvider = dataProvider_;
        auto processMsg = [] () mutable
        {
        };

        auto processSubset = [&subsets, &dataProvider]()
        {
            if (dataProvider->hasVariants())
            {
                subsets.insert(dataProvider->getSubset() +
                               "[" + std::to_string(dataProvider->variantId()) + "]");
            }
            else
            {
                subsets.insert(dataProvider->getSubset());
            }
        };

        auto processFinish = []()
        {
        };

        dataProvider_->run(QuerySet({}),
                           processMsg,
                           processSubset,
                           processFinish);

        return subsets;
    }

}  // namespace bufr
}  // namespace Ingester