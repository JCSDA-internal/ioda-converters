/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <array>
#include <unordered_map>

#include "QuerySet.h"
#include "ResultSet.h"
#include "DataProvider/DataProvider.h"
#include "DataProvider/SubsetVariant.h"
#include "Target.h"

namespace Ingester {
namespace bufr {

    /// \brief Manages the execution of queries against on a BUFR file.
    class QueryRunner
    {
     public:
        /// \brief Constructor.
        /// \param[in] querySet The set of queries to execute against the BUFR file.
        /// \param[in, out] resultSet The object used to store the accumulated collected data.
        /// \param[in] dataProvider The BUFR data provider to use.
        QueryRunner(const QuerySet& querySet,
                    ResultSet& resultSet,
                    const DataProviderType& dataProvider);

        /// \brief Run the queries against the currently open BUFR message subset. Collect the
        /// results into the ResultSet.
        void accumulate();

     private:
        const QuerySet querySet_;
        ResultSet& resultSet_;
        const DataProviderType& dataProvider_;
        std::unordered_map<SubsetVariant, Targets> targetCache_;

        /// \brief Look for the list of targets for the currently active BUFR message subset that
        /// apply to the QuerySet and cache them.
        /// \param[in, out] targets The list of targets to populate.
        void findTargets(Targets& targets);
    };
}  // namespace bufr
}  // namespace Ingester
