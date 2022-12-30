/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "WmoQueryPrinter.h"


#include <iostream>
#include <memory>
#include <unordered_map>
#include <sstream>

#include "eckit/exception/Exceptions.h"
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
        if (dataProvider_->isFileOpen())
        {
            std::ostringstream errStr;
            errStr << "Tried to call QueryPrinter::getQueries, but the file is already open!";
            throw eckit::BadParameter(errStr.str());
        }

        dataProvider_->open();

        std::unordered_map<SubsetVariant, std::vector<QueryData>> dataMap;

        auto& dataProvider = dataProvider_;
        auto processSubset = [&variant, &dataMap, &dataProvider]() mutable
        {
            auto subsetVariant = dataProvider->getSubsetVariant();
            if (subsetVariant == variant)
            {
                dataMap.insert({variant,SubsetTable(dataProvider).allQueryData()});
            }
        };

        dataProvider_->run(QuerySet({variant.subset}), processSubset);

        std::vector<QueryData> queryData;
        for (const auto& queryObjs : dataMap)
        {
            queryData.insert(queryData.end(), queryObjs.second.begin(), queryObjs.second.end());
        }

        dataProvider_->close();

        return queryData;
    }

    std::set<SubsetVariant> WmoQueryPrinter::getSubsetVariants() const
    {
        if (dataProvider_->isFileOpen())
        {
            std::ostringstream errStr;
            errStr << "Tried to call QueryPrinter::getSubsetVariants but the file is already open!";
            throw eckit::BadParameter(errStr.str());
        }

        dataProvider_->open();

        std::set<SubsetVariant> variants;
        auto& dataProvider = dataProvider_;

        auto processSubset = [&variants, &dataProvider]()
        {
            variants.insert(dataProvider->getSubsetVariant());
        };

        dataProvider_->run(QuerySet({}), processSubset);

        dataProvider_->close();

        return variants;
    }

}  // namespace bufr
}  // namespace Ingester