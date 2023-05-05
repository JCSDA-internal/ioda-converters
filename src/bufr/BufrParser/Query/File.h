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
        std::shared_ptr<DataProvider> dataProvider_;
    };
}  // namespace bufr
}  // namespace Ingester
