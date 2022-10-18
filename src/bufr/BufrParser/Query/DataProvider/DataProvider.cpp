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


namespace Ingester {
namespace bufr {
    void DataProvider::updateData(const std::string& subset, int bufrLoc)
    {
        bufrLoc_ = bufrLoc;
        int size = 0;
        int *intPtr = nullptr;
        double *dataPtr = nullptr;

        int strLen = 0;
        char *charPtr = nullptr;

        get_inode_f(bufrLoc, &inode_);
        get_nval_f(bufrLoc, &nval_);

        updateTableData(subset);

        get_val_f(bufrLoc, &dataPtr, &size);
        val_ = gsl::span<const double>(dataPtr, size);

        get_inv_f(bufrLoc, &intPtr, &size);
        inv_ = gsl::span<const int>(intPtr, size);

        subset_ = subset;
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

        nemdefs_f(FileUnit,
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

    std::set<std::string> DataProvider::getSubsets()
    {
        static const int SubsetLen = 9;
        int iddate;

        std::set<std::string> subsets;

        char subset[SubsetLen];
        while (ireadmg_f(FileUnit, subset, &iddate, SubsetLen) == 0)
        {
            auto str_subset = std::string(subset);
            str_subset.erase(
                remove_if(str_subset.begin(), str_subset.end(), isspace), str_subset.end());
            subsets.insert(str_subset);
        }

        return subsets;
    }
}  // namespace bufr
}  // namespace Ingester
