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

    std::vector<QueryData> WmoQueryPrinter::getQueries(const SubsetVariant& variant)
    {
        std::unordered_map<SubsetVariant, std::vector<QueryData>> dataMap;

        auto processMsg = []()
        {
        };

        auto& dataProvider = dataProvider_;
        auto processSubset = [&variant, &dataMap, &dataProvider]() mutable
        {
            auto subsetVariant = dataProvider->getSubsetVariant();
            if (subsetVariant == variant)
            {
                dataMap.insert({variant,SubsetTable(dataProvider).allQueryData()});
            }
        };

        auto processFinish = []()
        {
        };

        dataProvider_->run(QuerySet({variant.subset}),
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

    std::set<SubsetVariant> WmoQueryPrinter::getSubsetVariants() const
    {
        std::set<SubsetVariant> variants;

        auto& dataProvider = dataProvider_;
        auto processMsg = [] () mutable
        {
        };

        auto processSubset = [&variants, &dataProvider]()
        {
            variants.insert(dataProvider->getSubsetVariant());
        };

        auto processFinish = []()
        {
        };

        dataProvider_->run(QuerySet({}),
                           processMsg,
                           processSubset,
                           processFinish);

        return variants;
    }

}  // namespace bufr
}  // namespace Ingester