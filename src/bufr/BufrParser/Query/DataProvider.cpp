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
                auto typ = std::string(&charPtr[wordIdx * strLen], strLen);

                if      (typ == Subset)            typ_[wordIdx] = Typ::Subset;
                else if (typ == DelayedRep)        typ_[wordIdx] = Typ::DelayedRep;
                else if (typ == FixedRep)          typ_[wordIdx] = Typ::FixedRep;
                else if (typ == DelayedRepStacked) typ_[wordIdx] = Typ::DelayedRepStacked;
                else if (typ == DelayedBinary)     typ_[wordIdx] = Typ::DelayedBinary;
                else if (typ == Sequence)          typ_[wordIdx] = Typ::Sequence;
                else if (typ == Repeat)            typ_[wordIdx] = Typ::Repeat;
                else if (typ == StackedRepeat)     typ_[wordIdx] = Typ::StackedRepeat;
                else if (typ == Number)            typ_[wordIdx] = Typ::Number;
                else if (typ == Character)         typ_[wordIdx] = Typ::Character;
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

        int iret;
        TypeInfo info;
        nemdefs_f(fileUnit_,
                  getTag(idx).c_str(),
                  unitCStr,
                  UNIT_STR_LEN,
                  descCStr,
                  DESC_STR_LEN,
                  &iret);

        if (iret != 0)
        {
            std::ostringstream errMsg;
            errMsg << "Call to nembdefs_f failed for " << getTag(idx) << ".";
            throw eckit::BadParameter(errMsg.str());
        }

        // trim the unit string
        auto unitStr = std::string(unitCStr);
        size_t end = unitStr.find_last_not_of( " \n\r\t\f\v");
        unitStr = (end == std::string::npos) ? "" : unitStr.substr(0, end + 1);
        info.unit = unitStr;

        // trim the description string
        auto descStr = std::string(descCStr);
        end = descStr.find_last_not_of( " \n\r\t\f\v");
        descStr = (end == std::string::npos) ? "" : descStr.substr(0, end + 1);
        info.unit = descStr;

        nemspecs_f(fileUnit_,
                   getTag(idx).c_str(),
                   1,
                   &info.scale,
                   &info.reference,
                   &info.bits,
                   &iret);

        if (iret != 0)
        {
            std::ostringstream errMsg;
            errMsg << "Call to nemspecs_f failed for " << getTag(idx) << ".";
            throw eckit::BadParameter(errMsg.str());
        }

        return info;
    }
}  // namespace bufr
}  // namespace Ingester
