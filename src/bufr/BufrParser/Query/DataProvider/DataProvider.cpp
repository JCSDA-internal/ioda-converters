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
    void DataProvider::run(const QuerySet& querySet,
                           const std::function<void()> processSubset,
                           const std::function<void()> processMsg,
                           const std::function<bool()> continueProcessing)
    {
        if (!isOpen_)
        {
            std::ostringstream errStr;
            errStr << "Tried to call DataProvider::run, but the file is not open!";
            throw eckit::BadParameter(errStr.str());
        }

        static int SubsetLen = 9;
        char subsetChars[SubsetLen];
        int iddate;

        int bufrLoc;
        int il, im;  // throw away

        bool foundBufrMsg = false;
        bool foundBufrSubset = false;

        while (ireadmg_f(FileUnit, subsetChars, &iddate, SubsetLen) == 0)
        {
            foundBufrMsg = true;
            subset_ = std::string(subsetChars);
            subset_.erase(std::remove_if(subset_.begin(), subset_.end(), isspace), subset_.end());

            if (querySet.includesSubset(subset_))
            {
                while (ireadsb_f(FileUnit) == 0)
                {
                    foundBufrSubset = true;
                    status_f(FileUnit, &bufrLoc, &il, &im);
                    updateData(bufrLoc);

                    processSubset();
                    if (!continueProcessing()) break;
                }

                processMsg();
                if (!continueProcessing()) break;
            }
        }

        deleteData();

        if (!foundBufrMsg)
        {
            std::ostringstream errStr;
            errStr << "No BUFR messages were found! ";
            errStr << "Please make sure that " << filePath_ << " exists and is a valid BUFR file.";
            throw eckit::BadValue(errStr.str());
        }

        if (!foundBufrSubset)
        {
            std::ostringstream errStr;
            errStr << "No valid BUFR subsets were found from your queries! ";
            errStr << "Please make sure you are querying for valid subsets that exist in ";
            errStr << filePath_ << ". ";
            errStr << "Otherwise there might be a problem with the BUFR file (no subsets).";
            throw eckit::BadValue(errStr.str());
        }
    }

    void DataProvider::updateData(int bufrLoc)
    {
        bufrLoc_ = bufrLoc;
        int size = 0;
        int *intPtr = nullptr;
        double *dataPtr = nullptr;

        get_inode_f(bufrLoc, &inode_);
        get_nval_f(bufrLoc, &nval_);

        updateTableData(subset_);

        get_val_f(bufrLoc, &dataPtr, &size);
        val_ = gsl::span<const double>(dataPtr, size);

        get_inv_f(bufrLoc, &intPtr, &size);
        inv_ = gsl::span<const int>(intPtr, size);
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

            if (table_idx == 0) return info;

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
