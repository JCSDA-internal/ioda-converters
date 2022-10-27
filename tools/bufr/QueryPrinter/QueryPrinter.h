//
// Created by Ronald McLaren on 10/19/22.
//

#pragma once

#include <memory>

#include "../../../src/bufr/BufrParser/Query/DataProvider/DataProvider.h"
#include "../../../src/bufr/BufrParser/Query/DataProvider/NcepDataProvider.h"
#include "../../../src/bufr/BufrParser/Query/SubsetTable.h"


namespace Ingester {
namespace bufr {
    class QueryPrinter
    {
     public:
        QueryPrinter() = delete;

        explicit QueryPrinter(std::shared_ptr<DataProvider> dataProvider)
          : dataProvider_(dataProvider) {};

        void printQueries(const std::string& subset);

        virtual std::vector<QueryData> getQueries(const SubsetVariant& variant) = 0;

        /// \brief Get a complete set of subsets in the data file. WARNING: using this will be slow
        ///        and reset the file pointer.
        virtual std::set<SubsetVariant> getSubsetVariants() const = 0;

     protected:
        const int FileUnit = 12;
        std::shared_ptr<DataProvider> dataProvider_;

        std::vector<std::pair<int, std::string>> getDimPaths(
            const std::vector<Ingester::bufr::QueryData>& queryData);
        std::string dimStyledStr(int dims);
        std::string typeStyledStr(const Ingester::bufr::TypeInfo& info);
        void printDimPaths(std::vector<std::pair<int, std::string>> dimPaths);
        void printQueryList(const std::vector<Ingester::bufr::QueryData>& queries);
    };
}  // namespace bufr
}  // namespace Ingester
