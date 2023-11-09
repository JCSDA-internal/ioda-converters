/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "File.h"

#include <algorithm>

#include "bufr_interface.h"

#include "QueryRunner.h"
#include "QuerySet.h"
#include "DataProvider/DataProvider.h"
#include "DataProvider/NcepDataProvider.h"
#include "DataProvider/WmoDataProvider.h"


namespace Ingester {
namespace bufr {
    File::File(const std::string &filename, const std::string &wmoTablePath)
    {
        if (wmoTablePath.empty())
        {
            dataProvider_ = std::make_shared<Ingester::bufr::NcepDataProvider>(filename);
        }
        else
        {
            dataProvider_ = std::make_shared<Ingester::bufr::WmoDataProvider>(filename,
                                                                             wmoTablePath);
        }

        dataProvider_->open();
    }

    void File::close()
    {
        dataProvider_->close();
    }

    void File::rewind()
    {
        dataProvider_->rewind();
    }

    ResultSet File::execute(const QuerySet &querySet, size_t next)
    {
        size_t msgCnt = 0;
        auto resultSet = ResultSet();
        auto queryRunner = QueryRunner(querySet, resultSet, dataProvider_);

        auto processMsg = [&msgCnt] () mutable
        {
            msgCnt++;
        };

        auto processSubset = [&queryRunner]() mutable
        {
            queryRunner.accumulate();
        };

        auto continueProcessing = [next, &msgCnt]() -> bool
        {
            if (next > 0)
            {
               return  msgCnt < next;
            }

            return true;
        };

        dataProvider_->run(querySet,
                           processSubset,
                           processMsg,
                           continueProcessing);

        return resultSet;
    }
}  // namespace bufr
}  // namespace Ingester
