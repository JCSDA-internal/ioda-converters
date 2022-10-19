//
// Created by Ronald McLaren on 10/5/22.
//

#include "WmoDataProvider.h"

#include "bufr_interface.h"

#include <gsl/gsl-lite.hpp>
#include <algorithm>
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

    void WmoDataProvider::run(const QuerySet& querySet,
                              const std::function<void()> processMsg,
                              const std::function<void()> processSubset,
                              const std::function<void()> processFinish,
                              const std::function<bool()> continueProcessing)
    {
        static int SubsetLen = 9;
        unsigned int messageNum = 0;
        char subsetChars[SubsetLen];
        int iddate;

        int bufrLoc;
        int il, im;  // throw away

        while (ireadmg_f(FileUnit, subsetChars, &iddate, SubsetLen) == 0)
        {
            auto subset = std::string(subsetChars);
            subset.erase(std::remove_if(subset.begin(), subset.end(), isspace), subset.end());

            if (querySet.includesSubset(subset))
            {
                while (ireadsb_f(FileUnit) == 0)
                {
                    status_f(FileUnit, &bufrLoc, &il, &im);
                    updateData(subset, bufrLoc);

                    processSubset();
                }

                processMsg();
                if (!continueProcessing()) break;
            }
        }

        processFinish();
        deleteData();
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
        deleteData();

        struct TagData
        {
            char* ptr;
            int strLen;
            int size;
            std::string tagStr;
        };

        TagData tagData;

        get_tag_f(&tagData.ptr, &tagData.strLen, &tagData.size);
        tagData.tagStr = std::string(&tagData.ptr[0], tagData.size * tagData.strLen);

        auto tableData = std::make_shared<TableData>();
        if (tableCache_.find(tagData.tagStr) == tableCache_.end())
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

            tableData->tag.resize(tagData.size);
            for (int wordIdx = 0; wordIdx < tagData.size; wordIdx++)
            {
                auto tag = std::string(&tagData.ptr[wordIdx * tagData.strLen], tagData.strLen);
                tableData->tag[wordIdx] = tag.substr(0, tag.find_first_of(' '));
            }

            get_jmpb_f(&intPtr, &size);
            tableData->jmpb = std::vector<int>(intPtr, intPtr + size);
            tableCache_[tagData.tagStr] = tableData;
        }

        tableData = tableCache_[tagData.tagStr];

        return tableData;
    }
}  // namespace bufr
}  // namespace Ingester
