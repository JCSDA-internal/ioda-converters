/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "QueryPrinter.h"


namespace Ingester {
namespace bufr {

    class QueryData;

    class WmoQueryPrinter : public QueryPrinter
    {
     public:
       WmoQueryPrinter(const std::string& filepath, const std::string& tablepath);
       ~WmoQueryPrinter() = default;

        /// \brief Get the query data for a specific subset variant type
        /// \param variant The subset variant
        /// \returns Vector of SubsetTable QueryData objects
        std::vector<QueryData> getQueries(const SubsetVariant& variant) final;

        /// \brief Get a complete set of subsets in the data file. WARNING: using this will be slow
        ///        and reset the file pointer.
        /// \returns Vector of subset variants
        std::set<SubsetVariant> getSubsetVariants() const final;

     private:
        const int FileUnitTable1 = 13;
        const int FileUnitTable2 = 14;
    };
}  // namespace bufr
}  // namespace Ingester
