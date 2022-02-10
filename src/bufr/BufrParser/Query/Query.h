//
// Created by rmclaren on 1/28/22.
//

#pragma once

#include <string>
#include <vector>
#include <array>
#include <map>

#include "QuerySet.h"
#include "ResultSet.h"
#include "DataProvider.h"

namespace Ingester {
namespace bufr {
    namespace __details
    {
        struct Target {
            std::string name;
            std::string queryStr;
            bool isString;
            std::vector<int> seqPath;
            std::vector<int> nodeIds;
            std::vector<std::string> dimPaths;
            std::vector<int> exportDimIdxs;
        };

        template <typename T>
        class OffsetArray
        {
         public:
            OffsetArray(size_t startIdx, size_t endIdx)
                : offset_(startIdx)
            {
                data_.resize(endIdx - startIdx + 1);
            }

            T& operator[](size_t idx) { return data_[idx - offset_]; }

         private:
            std::vector<T> data_;
            size_t offset_;
        };

        struct ProcessingMasks {
            std::vector<bool> valueNodeMask;
            std::vector<bool> pathNodeMask;
        };
    }  // namespace __details

    class Query
    {
     public:
        Query(const QuerySet& querySet, ResultSet& resultSet);
        void query(const std::string& subset, int bufrLoc);

     private:
        const QuerySet querySet_;
        std::map<std::string, std::shared_ptr<std::vector<__details::Target>>> targetCache_;
        std::map<std::string, std::shared_ptr<__details::ProcessingMasks>> maskCache_;

        int fortranFileUnit_;
        int bufrLoc_;
        ResultSet& resultSet_;

        void findTargets(const std::string& subset,
                         std::shared_ptr<std::vector<__details::Target>>& targets,
                         std::shared_ptr<__details::ProcessingMasks>& masks);

        __details::Target findTarget(const std::string& subset,
                                     const std::string& targetName,
                                     const std::string& query) const;

        bool isQueryNode(int nodeIdx) const;

        void getDimInfo(const std::vector<int>& branches,
                        int mnemonicCursor,
                        std::vector<std::string>& dimPaths,
                        std::vector<int>& dimIdxs) const;

        void collectData(std::shared_ptr<std::vector<__details::Target>> targets,
                         std::shared_ptr<__details::ProcessingMasks> masks,
                         ResultSet& resultSet) const;
        };
}  // namespace bufr
}  // namespace Ingester

