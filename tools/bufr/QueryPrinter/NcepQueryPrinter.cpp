/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "NcepQueryPrinter.h"

#include <memory>
#include <iostream>
#include <sstream>

#include "eckit/exception/Exceptions.h"

#include "QueryPrinter.h"
#include "../../../src/bufr/BufrParser/Query/DataProvider/NcepDataProvider.h"
#include "../../../src/bufr/BufrParser/Query/SubsetTable.h"

namespace Ingester {
namespace bufr {

    NcepQueryPrinter::NcepQueryPrinter(const std::string& filepath) :
      QueryPrinter(std::make_shared<NcepDataProvider>(filepath))
    {
    }

    std::shared_ptr<SubsetTable> NcepQueryPrinter::getQueries(const SubsetVariant& variant)
    {
        if (dataProvider_->isFileOpen())
        {
            std::ostringstream errStr;
            errStr << "Tried to call QueryPrinter::getQueries, but the file is already open!";
            throw eckit::BadParameter(errStr.str());
        }

        bool finished = false;

        dataProvider_->open();

        std::shared_ptr<SubsetTable> subsetTable;
        auto& dataProvider = dataProvider_;
        auto processSubset = [&subsetTable, &finished, &dataProvider]() mutable
        {
            subsetTable = std::make_shared<SubsetTable>(dataProvider);
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

        dataProvider_->close();

        return subsetTable;
    }

    std::set<SubsetVariant> NcepQueryPrinter::getSubsetVariants() const
    {
        if (dataProvider_->isFileOpen())
        {
            std::ostringstream errStr;
            errStr << "Tried to call QueryPrinter::getSubsetVariants but the file is already open!";
            throw eckit::BadParameter(errStr.str());
        }

        dataProvider_->open();

        std::set<SubsetVariant> subsets;

        auto& dataProvider = dataProvider_;
        auto processMsg = [&subsets, &dataProvider] () mutable
        {
            subsets.insert(dataProvider->getSubsetVariant());
        };

        dataProvider_->run(QuerySet({}),
                           [](){},
                           processMsg);

        dataProvider_->close();

        return subsets;
    }
}  // namespace bufr
}  // namespace Ingester