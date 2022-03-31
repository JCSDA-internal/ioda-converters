/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>

#include "QuerySet.h"
#include "ResultSet.h"

namespace Ingester {
namespace bufr {
    class File
    {
    public:
        File() = delete;

        File(const std::string &filename,
             bool isWmoFormat = false,
             const std::string &wmoTablePath = "");

        ResultSet execute(const QuerySet &query_set, int next = 0);

        void open();
        void close();
        void rewind();

    private:
        bool isWmoFormat_;
        int fileUnit_;
        int fileUnitTable1_;
        int fileUnitTable2_;
        std::string filename_;
        std::string wmoTablePath_;

        int nextFileUnit();
    };
}  // namespace bufr
}  // namespace Ingester
