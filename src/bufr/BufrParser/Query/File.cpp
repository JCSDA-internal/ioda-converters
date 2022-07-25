/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "File.h"

#include "bufr_interface.h"

#include "Query.h"
#include "QuerySet.h"
#include "DataProvider.h"


namespace Ingester {
namespace bufr {
    File::File(const std::string &filename, bool isWmoFormat, const std::string &wmoTablePath) :
            filename_(filename),
            fileUnit_(nextFileUnit()),
            fileUnitTable1_(nextFileUnit()),
            fileUnitTable2_(nextFileUnit()),
            isWmoFormat_(isWmoFormat),
            wmoTablePath_(wmoTablePath)
    {
        open();
    }

    void File::open()
    {
        open_f(fileUnit_, filename_.c_str());

        if (!isWmoFormat_)
        {
            openbf_f(fileUnit_, "IN", fileUnit_);
        }
        else
        {
            openbf_f(fileUnit_, "SEC3", fileUnit_);

            if (!wmoTablePath_.empty())
            {
                mtinfo_f(wmoTablePath_.c_str(), fileUnitTable1_, fileUnitTable2_);
            }
        }
    }

    void File::close()
    {
        closbf_f(fileUnit_);
        close_f(fileUnit_);
    }

    void File::rewind() {
        close();
        open();
    }

    ResultSet File::execute(const QuerySet &querySet, size_t next)
    {
        static int SubsetLen = 9;
        unsigned int messageNum = 0;
        char subset[SubsetLen];
        int iddate;

        int bufrLoc;
        int il, im;  // throw away

        auto dataProvider = DataProvider();

        auto resultSet = ResultSet(querySet.names());
        auto query = Query(querySet, resultSet, dataProvider);

        while (ireadmg_f(fileUnit_, subset, &iddate, SubsetLen) == 0)
        {
            while (ireadsb_f(fileUnit_) == 0)
            {
                status_f(fileUnit_, &bufrLoc, &il, &im);
                dataProvider.updateData(bufrLoc);
                query.query();
            }

            if (next > 0 && ++messageNum >= next) break;
        }

        resultSet.setTargets(query.getTargets());

        dataProvider.deleteData();

        return resultSet;
    }

    int File::nextFileUnit()
    {
        static int lastFileUnit = 11;  // Numbers 12 and above are valid.
        return ++lastFileUnit;
    }
}  // namespace bufr
}  // namespace Ingester