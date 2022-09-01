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
#include <unordered_map>

#include "eckit/exception/Exceptions.h"

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
    const char* Number = "NUM";
    const char* Character = "CHR";
}  // namespace


namespace Ingester {
namespace bufr {

    void DataProvider::updateData(int bufrLoc)
    {
        bufrLoc_ = bufrLoc;
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
                static const std::unordered_map<std::string, Typ> TypMap =
                    {{Subset,            Typ::Subset},
                     {DelayedRep,        Typ::DelayedRep},
                     {FixedRep,          Typ::FixedRep},
                     {DelayedRepStacked, Typ::DelayedRepStacked},
                     {DelayedBinary,     Typ::DelayedBinary},
                     {Sequence,          Typ::Sequence},
                     {Repeat,            Typ::Repeat},
                     {StackedRepeat,     Typ::StackedRepeat},
                     {Number,            Typ::Number},
                     {Character,         Typ::Character}};

                auto typ = std::string(&charPtr[wordIdx * strLen], strLen);
                typ_[wordIdx] = TypMap.at(typ);
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

        subset_ = getTag(getInode());
    }

    void DataProvider::deleteData()
    {
        delete_table_data_f();
    }

    TypeInfo DataProvider::getTypeInfo(FortranIdx idx) const
    {
        static const unsigned int UNIT_STR_LEN = 24;
        static const unsigned int DESC_STR_LEN = 55;

        char unitCStr[UNIT_STR_LEN];
        char descCStr[DESC_STR_LEN];

        int retVal;
        TypeInfo info;

        nemdefs_f(fileUnit_,
                  getTag(idx).c_str(),
                   unitCStr,
                   UNIT_STR_LEN,
                   descCStr,
                   DESC_STR_LEN,
                   &retVal);

        if (retVal == 0)
        {
            // trim the unit string
            auto unitStr = std::string(unitCStr);
            size_t end = unitStr.find_last_not_of(" \n\r\t\f\v");
            unitStr = (end == std::string::npos) ? "" : unitStr.substr(0, end + 1);
            info.unit = unitStr;

            // trim the unit string
            auto descStr = std::string(descCStr);
            end = descStr.find_last_not_of(" \n\r\t\f\v");
            descStr = (end == std::string::npos) ? "" : descStr.substr(0, end + 1);
            info.description = descStr;

            int descriptor;
            int table_idx;
            char table_type;

            nemtab_f(bufrLoc_,
                     getTag(idx).c_str(),
                     &descriptor,
                     &table_type,
                     &table_idx);

            nemtbb_f(bufrLoc_,
                     table_idx,
                     unitCStr,
                     UNIT_STR_LEN,
                     &info.scale,
                     &info.reference,
                     &info.bits);
        }

        return info;
    }
}  // namespace bufr
}  // namespace Ingester
