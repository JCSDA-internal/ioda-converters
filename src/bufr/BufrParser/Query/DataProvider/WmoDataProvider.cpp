//
// Created by Ronald McLaren on 10/5/22.
//

#include "WmoDataProvider.h"

#include "bufr_interface.h"

#include <gsl/gsl-lite.hpp>
#include <unordered_map>
#include <vector>
#include <iostream>

namespace Ingester {
namespace bufr {
    WmoDataProvider::WmoDataProvider(const std::string& filePath,
                                     const std::string& tableFilePath) :
      DataProvider(filePath),
      tableFilePath_(tableFilePath),
      currentTableData_(nullptr)
    {
    }

    void WmoDataProvider::run(const QuerySet&,
                              const std::function<void(const DataProviderType&)> processMsg,
                              const std::function<void(const DataProviderType&)> processSubset,
                              const std::function<void(const DataProviderType&)> processFinish,
                              const std::function<bool(const DataProviderType&)> continueProcessing)
    {
    }

    void WmoDataProvider::open()
    {
        open_f(FileUnit, filePath_.c_str());
        openbf_f(FileUnit, "SEC3", FileUnit);
        mtinfo_f(tableFilePath_.c_str(), FileUnitTable1, FileUnitTable2);
    }

    void WmoDataProvider::updateTableData(const std::string& subset)
    {
        if (currentTableData_ == nullptr || subset != currentTableData_->subset)
        {
            currentTableData_ = getTableData(subset);
        }
    }

    std::shared_ptr<TableData> WmoDataProvider::getTableData(const std::string& subset)
    {
        auto tableData = std::make_shared<TableData>();
        if (tableCache_.find(subset) == tableCache_.end())
        {
            int size = 0;
            int *intPtr = nullptr;
            double *dataPtr = nullptr;
            int strLen = 0;
            char *charPtr = nullptr;

            get_isc_f(&intPtr, &size);
            tableData->isc = std::vector<int>(intPtr, intPtr + size);

            get_link_f(&intPtr, &size);
            tableData->link = std::vector<int>(intPtr, intPtr + size);

            get_itp_f(&intPtr, &size);
            tableData->itp = std::vector<int>(intPtr, intPtr + size);

            get_typ_f(&charPtr, &strLen, &size);
            tableData->typ.resize(size);
            for (int wordIdx = 0; wordIdx < size; wordIdx++)
            {
                auto typ = std::string(&charPtr[wordIdx * strLen], strLen);
                tableData->typ[wordIdx] = TypMap.at(typ);
            }

            get_tag_f(&charPtr, &strLen, &size);
            tableData->tag.resize(size);
            for (int wordIdx = 0; wordIdx < size; wordIdx++)
            {
                auto tag = std::string(&charPtr[wordIdx * strLen], strLen);
                tableData->tag[wordIdx] = tag.substr(0, tag.find_first_of(' '));
            }

            get_jmpb_f(&intPtr, &size);
            tableData->jmpb = std::vector<int>(intPtr, intPtr + size);

            tableCache_[subset] = tableData;
        }

        return tableCache_[subset];
    }
}  // namespace bufr
}  // namespace Ingester
