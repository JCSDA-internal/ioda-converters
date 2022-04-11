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

        File(const std::string& filename,
             bool isWmoFormat = false,
             const std::string& wmoTablePath = "");

        ResultSet execute(const QuerySet& query_set, size_t next = 0);

        void open();
        void close();
        void rewind();

    private:
        const std::string filename_;
        const int fileUnit_;
        const int fileUnitTable1_;
        const int fileUnitTable2_;
        const bool isWmoFormat_;
        const std::string wmoTablePath_;

        int nextFileUnit();
    };
}  // namespace bufr
}  // namespace Ingester
