//
// Created by rmclaren on 7/1/21.
//

#include "File.h"

#include "bufr_interface.h"

#include "Query.h"
#include "QuerySet.h"
#include "DataProvider.h"


namespace Ingester {
namespace bufr {
    File::File(const std::string &filename, bool isWmoFormat, const std::string &wmoTablePath) :
            fileUnit_(nextFileUnit()),
            fileUnitTable1_(0),
            fileUnitTable2_(0),
            filename_(filename),
            isWmoFormat_(isWmoFormat),
            wmoTablePath_(wmoTablePath)
    {
        if (isWmoFormat && !wmoTablePath_.empty())
        {
            fileUnitTable1_ = nextFileUnit();
            fileUnitTable2_ = nextFileUnit();
        }

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

    ResultSet File::execute(const QuerySet &querySet, int next)
    {
        static int SubsetLen = 9;
        unsigned int messageNum = 0;
        char subset[SubsetLen];
        int iddate;

        int bufrLoc;
        int il, im; // throw away

        auto dataProvider = DataProvider::instance();
        dataProvider->loadTableInfo();

        auto resultSet = ResultSet(querySet.names());
        auto query = Query(querySet, resultSet);

        while (ireadmg_f(fileUnit_, subset, &iddate, SubsetLen) == 0)
        {
            while (ireadsb_f(fileUnit_) == 0)
            {
                status_f(fileUnit_, &bufrLoc, &il, &im);

                dataProvider->loadDataInfo(bufrLoc);
                query.query(std::string(subset), bufrLoc);
            }

            if (next > 0 && ++messageNum >= next) break;
        }

        dataProvider->deleteTableInfo();

        return resultSet;
    }

    int File::nextFileUnit()
    {
        static int lastFileUnit = 11; //  Numbers 12 and above are valid.
        return ++lastFileUnit;
    }
}  // namespace bufr
}  // namespace Ingester
