/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>

#include "../../../src/bufr/BufrParser/Query/DataProvider/DataProvider.h"
#include "../../../src/bufr/BufrParser/Query/DataProvider/NcepDataProvider.h"
#include "../../../src/bufr/BufrParser/Query/SubsetTable.h"


namespace Ingester {
namespace bufr {
    /// \brief Responsible for printing formatted query data out to stdio.
    class QueryPrinter
    {
     public:
        QueryPrinter() = delete;

        explicit QueryPrinter(std::shared_ptr<DataProvider> dataProvider);

        /// \brief Print queries associated with the given subset to stdio.
        /// \param subset The subset
        void printQueries(const std::string& subset);

        /// \brief Get the query data for a specific subset variant type
        /// \param variant The subset variant
        /// \returns Vector of SubsetTable QueryData objects
        virtual std::vector<QueryData> getQueries(const SubsetVariant& variant) = 0;

        /// \brief Get a complete set of subsets in the data file.
        /// \returns Vector of subset variants
        virtual std::set<SubsetVariant> getSubsetVariants() const = 0;

     protected:
        const int FileUnit = 12;
        std::shared_ptr<DataProvider> dataProvider_;

        /// \brief Get the dimension paths for the given query data objects
        /// \param queryData Vector of QueryData objects
        /// \returns Vector of number of dimension/query sub-path string pairs.
        std::vector<std::pair<int, std::string>> getDimPaths(
            const std::vector<Ingester::bufr::QueryData>& queryData);

        /// \brief Create a styled string for the dimension (ex: 2d)
        /// \param dims The number of dimensions
        /// \returns The dimension string for the output
        std::string dimStyledStr(int dims);

        /// \brief Create the styled string for the type (ex: float) given
        ///        the TypeInfo object for a field.
        /// \param info The TypeInfo object for the field in question
        /// \returns Styled string for the type of the field.
        std::string typeStyledStr(const Ingester::bufr::TypeInfo& info);

        /// \brief Print the list of dimensioning sub-paths to stdioo
        /// \param dimPaths Vector of piars of number of dimensions to sub-path string
        /// \returns Styled string for the type of the field.
        void printDimPaths(std::vector<std::pair<int, std::string>> dimPaths);

        /// \brief Print the list of possible queries out to stdio
        /// \param queries Vector of QueryData objects for each available query.
        void printQueryList(const std::vector<Ingester::bufr::QueryData>& queries);
    };
}  // namespace bufr
}  // namespace Ingester
