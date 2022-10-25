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
        int varientNumber;
    };

    class WmoDataProvider : public DataProvider
    {
     public:
        WmoDataProvider(const std::string& filePath_,
                        const std::string& tableFilePath_);

        void open() final;

        // Getters to get the raw data by idx. Since fortran indices are 1-based,
        // we need to subtract 1 to get the correct c style index.
        inline FortranIdx getIsc(FortranIdx idx) const { return currentTableData_->isc[idx - 1]; }
        inline FortranIdx getLink(FortranIdx idx) const { return currentTableData_->link[idx - 1]; }
        inline FortranIdx getItp(FortranIdx idx) const { return currentTableData_->itp[idx - 1]; }
        inline FortranIdx getJmpb(FortranIdx idx) const { return currentTableData_->jmpb[idx - 1]; }
        inline Typ getTyp(FortranIdx idx) const { return currentTableData_->typ[idx - 1]; }
        inline std::string getTag(FortranIdx idx) const { return currentTableData_->tag[idx - 1]; }

        size_t variantId() const final;
        bool hasVariants() const final;

    private:
        typedef std::pair<std::string, size_t> Variant;

        const static int FileUnitTable1 = 13;
        const static int FileUnitTable2 = 14;

        const std::string tableFilePath_;
        std::unordered_map<std::string, std::shared_ptr<TableData>> tableCache_;
        std::shared_ptr<TableData> currentTableData_ = nullptr;

        std::vector<Variant> variants_;
        std::set<Variant> existingVariants;
        std::unordered_map<std::string, size_t> variantCount_;

        void updateTableData(const std::string& subset) final;

        void initialize();
        void _deleteData() final;
    };

}  // namespace bufr
}  // namespace Ingester