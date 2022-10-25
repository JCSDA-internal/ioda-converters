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

    void WmoDataProvider::open()
    {
        open_f(FileUnit, filePath_.c_str());
        openbf_f(FileUnit, "SEC3", FileUnit);
        mtinfo_f(tableFilePath_.c_str(), FileUnitTable1, FileUnitTable2);

        if (tableCache_.empty())
        {
            initialize();
        }
    }

    void WmoDataProvider::updateTableData(const std::string& subset)
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

        std::shared_ptr<TableData> tableData;
        if (tableCache_.find(tagData.tagStr) == tableCache_.end())
        {
            int size = 0;
            int *intPtr = nullptr;
            double *dataPtr = nullptr;
            int strLen = 0;
            char *charPtr = nullptr;

            tableData = std::make_shared<TableData>();

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

            // Figure out the variant ID.
            int variantNum = 1;
            for (const auto& cacheEntry : tableCache_)
            {
                if (cacheEntry.second->subset == subset) ++variantNum;
            }
            tableData->varientNumber = variantNum;

            tableCache_[tagData.tagStr] = tableData;

            if (variantCount_.find(subset) == variantCount_.end())
            {
                variantCount_.insert({subset, 0});
            }
            variantCount_.at(subset) += 1;
        }

        currentTableData_ = tableCache_[tagData.tagStr];
    }

    size_t WmoDataProvider::variantId() const
    {
        return currentTableData_->varientNumber;
    }

    bool  WmoDataProvider::hasVariants() const
    {
        return variantCount_.at(getSubset()) > 1;
    }

    void WmoDataProvider::initialize()
    {
        // Run the entire file in order to initialize the cache
        auto processMsg = []() mutable
        {
        };

        auto processSubset = [this]()
        {
//            auto variant = std::make_pair(getSubset(), variantId());
//
//            if (existingVariants.find(variant) == existingVariants.end())
//            {
//                existingVariants.insert(variant);
//                variantCount_.at(getSubset()) = 0;
//            }
//
//            variantCount_.at(getSubset()) += 1;
//            variants_.push_back(variant);
        };

        auto processFinish = [this]()
        {
        };

        run(QuerySet({}),
                       processMsg,
                       processSubset,
                       processFinish);

        rewind();
    }

    void WmoDataProvider::_deleteData()
    {
    }
}  // namespace bufr
}  // namespace Ingester
