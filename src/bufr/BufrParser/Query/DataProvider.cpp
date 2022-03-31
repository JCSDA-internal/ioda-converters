/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "DataProvider.h"
#include "bufr_interface.h"

#include <algorithm>
#include <iostream>

namespace
{
    const char* Subset = "SUB";
    const char* DelayedRep = "DRP";
    const char* FixedRep = "REP";
    const char* DelayedRepStacked = "DRS";
    const char* DelayedBinary = "DRB";
    const char* Sequence = "SEQ";
    const char* Repeat = "RPC";
    const char* StackedRepeat = "RPS";
}


namespace Ingester {
namespace bufr {

    std::shared_ptr<DataProvider> DataProvider::instance()
    {
        static auto instance = std::make_shared<DataProvider>();
        return instance;
    }

    void DataProvider::loadTableInfo()
    {
        int size = 0;
        int strLen = 0;
        int *intPtr = nullptr;
        char *charPtr = nullptr;
    }

    void DataProvider::loadDataInfo(int bufrLoc)
    {
        int size = 0;
        int *intPtr = nullptr;
        double *dataPtr = nullptr;

        int strLen = 0;
        char *charPtr = nullptr;

        if (isc_.empty())
        {
            get_isc_f(&intPtr, &size);
            isc_ = gsl::span<const int>(intPtr, size);

            get_link_f(&intPtr, &size);
            link_ = gsl::span<const int>(intPtr, size);

            get_itp_f(&intPtr, &size);
            itp_ = gsl::span<const int>(intPtr, size);

            get_typ_f(&charPtr, &strLen, &size);
            typ_.resize(size);
            for (int wordIdx = 0; wordIdx < size; wordIdx++)
            {
                auto typ = std::string(&charPtr[wordIdx * strLen], strLen);

                if      (typ == Subset)            typ_[wordIdx] = Typ::Subset;
                else if (typ == DelayedRep)        typ_[wordIdx] = Typ::DelayedRep;
                else if (typ == FixedRep)          typ_[wordIdx] = Typ::FixedRep;
                else if (typ == DelayedRepStacked) typ_[wordIdx] = Typ::DelayedRepStacked;
                else if (typ == DelayedBinary)     typ_[wordIdx] = Typ::DelayedBinary;
                else if (typ == Sequence)          typ_[wordIdx] = Typ::Sequence;
                else if (typ == Repeat)            typ_[wordIdx] = Typ::Repeat;
                else if (typ == StackedRepeat)     typ_[wordIdx] = Typ::StackedRepeat;
            }

            get_tag_f(&charPtr, &strLen, &size);
            tag_.resize(size);
            for (int wordIdx = 0; wordIdx < size; wordIdx++)
            {
                auto tag = std::string(&charPtr[wordIdx * strLen], strLen);
                tag_[wordIdx] = tag.substr(0, tag.find_first_of(' '));
            }

            get_jmpb_f(&intPtr, &size);
            jmpb_ = gsl::span<const int>(intPtr, size);
        }

        get_inode_f(bufrLoc, &inode_);
        get_nval_f(bufrLoc, &nval_);

        get_val_f(bufrLoc, &dataPtr, &size);
        val_ = gsl::span<const double>(dataPtr, size);

        get_inv_f(bufrLoc, &intPtr, &size);
        inv_ = gsl::span<const int>(intPtr, size);
    }

    void DataProvider::deleteTableInfo()
    {
        delete_table_data_f();
    }
}  // namespace bufr
}  // namespace Ingester