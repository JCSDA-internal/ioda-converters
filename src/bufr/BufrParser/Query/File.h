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

    /// \breif Manages an open BUFR file.
    class File
    {
     public:
        File() = delete;

        File(const std::string& filename,
             bool isWmoFormat = false,
             const std::string& wmoTablePath = "");

        /// \brief Execute the queries given in the query set over the BUFR file and accumulate the
        /// resulting data in the ResultSet.
        /// \param query_set The queryset object that contains the collection of desired queries
        /// \param next The number of messages worth of data to run. 0 reads all messages in the
        /// file.
        ResultSet execute(const QuerySet& query_set, size_t next = 0);

        /// \brief Close the currently opened BUFR file.
        void close();

        /// \brief Rewind the currently opened BUFR file to the beginning.
        void rewind();

     private:
        const std::string filename_;
        const int fileUnit_;
        const int fileUnitTable1_;
        const int fileUnitTable2_;
        const bool isWmoFormat_;
        const std::string wmoTablePath_;


        /// \brief Open the BUFR file whose parameters where given in the constructor.
        void open();

        /// \brief Get the next available Fortran file unit number.
        int nextFileUnit();
    };
}  // namespace bufr
}  // namespace Ingester
