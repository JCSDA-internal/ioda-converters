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
#include <unordered_set>
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

    SubsetTableType WmoQueryPrinter::getTable(const SubsetVariant& variant)
    {
        if (dataProvider_->isFileOpen())
        {
            std::ostringstream errStr;
            errStr << "Tried to call QueryPrinter::getTable, but the file is already open!";
            throw eckit::BadParameter(errStr.str());
        }

        dataProvider_->open();

        std::unordered_map<SubsetVariant, BufrNodeVector> dataMap;

        std::unordered_set<std::string> knownQueries;

        size_t maxLeaves = 0;
        std::shared_ptr<SubsetTable> subsetTable;
        auto& dataProvider = dataProvider_;
        auto processSubset = [&variant, &subsetTable, &maxLeaves, &dataProvider]() mutable
        {
            auto subsetVariant = dataProvider->getSubsetVariant();
            if (subsetVariant == variant)
            {
                auto thisTable = std::make_shared<SubsetTable>(dataProvider);
                auto leaves = thisTable->getLeaves();

                // Unfortunately the subsets for a variant are sometimes inconsistent
                // (have more queries than others) so we need to pick the largest one.
                if (leaves.size() > maxLeaves)
                {
                    maxLeaves = leaves.size();
                    subsetTable = thisTable;
                }
            }
        };

        dataProvider_->run(QuerySet({variant.subset}), processSubset);
        dataProvider_->close();

        return subsetTable;
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