//
// Created by Ronald McLaren on 10/5/22.
//

#pragma once

#include "DataProvider.h"

#include <string>
#include <unordered_map>
#include <memory>
#include <gsl/gsl-lite.hpp>

#include "../QuerySet.h"


namespace Ingester {
namespace bufr {

    struct TableData
    {
        // Table data;
        std::string subset;
        std::vector<int> isc;
        std::vector<int> link;
        std::vector<int> itp;
        std::vector<int> jmpb;
        std::vector<Typ> typ;
        std::vector<std::string> tag;
    };

    class WmoDataProvider : public DataProvider
    {
     public:
        WmoDataProvider(const std::string& filePath_,
                        const std::string& tableFilePath_);

        void run(const QuerySet& querySet,
                 const std::function<void(const DataProviderType&)> processMsg,
                 const std::function<void(const DataProviderType&)> processSubset,
                 const std::function<void(const DataProviderType&)> processFinish,
                 const std::function<bool(const DataProviderType&)> continueProcessing) final;

        void open() final;

        // Getters to get the raw data by idx. Since fortran indices are 1-based,
        // we need to subtract 1 to get the correct c style index.
        inline FortranIdx getIsc(FortranIdx idx) const { return currentTableData_->isc[idx - 1]; }
        inline FortranIdx getLink(FortranIdx idx) const { return currentTableData_->link[idx - 1]; }
        inline FortranIdx getItp(FortranIdx idx) const { return currentTableData_->itp[idx - 1]; }
        inline FortranIdx getJmpb(FortranIdx idx) const { return currentTableData_->jmpb[idx - 1]; }
        inline Typ getTyp(FortranIdx idx) const { return currentTableData_->typ[idx - 1]; }
        inline std::string getTag(FortranIdx idx) const { return currentTableData_->tag[idx - 1]; }

    private:
        const static int FileUnitTable1 = 13;
        const static int FileUnitTable2 = 14;

        const std::string tableFilePath_;
        std::unordered_map<std::string, std::shared_ptr<TableData>> tableCache_;
        std::shared_ptr<TableData> currentTableData_ = nullptr;

        void updateTableData(const std::string& subset) final;
        std::shared_ptr<TableData> getTableData(const std::string& subset);
    };

}  // namespace bufr
}  // namespace Ingester