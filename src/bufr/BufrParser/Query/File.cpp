//
// Created by rmclaren on 7/1/21.
//

#include "File.h"

#include "FortranObject.h"
#include "bufr_interface.h"

#include "Query.h"
#include "QuerySet.h"
#include "BufrDataManager.h"


extern "C"
{
    void __query_interface_MOD_execute_c(int, bufr::Address, int, bufr::Address);
}

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

    void File::open() {
        open_f(fileUnit_, filename_.c_str());

        if (!isWmoFormat_) {
            openbf_f(fileUnit_, "IN", fileUnit_);
        } else {
            openbf_f(fileUnit_, "SEC3", fileUnit_);

            if (!wmoTablePath_.empty()) {
                mtinfo_f(wmoTablePath_.c_str(), fileUnitTable1_, fileUnitTable2_);
            }
        }
    }

    void File::close() {
        closbf_f(fileUnit_);
        close_f(fileUnit_);
    }

    void File::rewind() {
        close();
        open();
    }

    ResultSet File::execute(const QuerySet &querySet, int next)
    {
        unsigned int messageNum = 0;
        char subset[10];
        int subsetLen = 10;
        int iddate;

        BufrDataManager bufrDataManager;

        auto query = Query(querySet);
        auto resultSet = ResultSet();
        while (ireadmg_f(fileUnit_, subset, &iddate, subsetLen) == 0)
        {
            while (ireadsb_f(fileUnit_) == 0)
            {
                query.query();
                return resultSet;
            }

            if (next > 0 && ++messageNum >= next) break;
        }

        return resultSet;
    }

    int File::nextFileUnit() {
        static int lastFileUnit = 11; //  Numbers 12 and above are valid.
        return ++lastFileUnit;
    }
}  // namespace bufr
}  // namespace Ingester
