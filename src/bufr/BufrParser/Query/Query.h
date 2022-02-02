//
// Created by rmclaren on 1/28/22.
//

#pragma once

#include <string>
#include <vector>
#include <map>

#include "QuerySet.h"
#include "ResultSet.h"
#include "BufrDataManager.h"

namespace Ingester {
    namespace bufr
    {
        namespace __details
        {
            struct Target {
                std::string name;
                std::string query_str;
                bool is_string;
                std::vector<int> seq_path;
                std::vector<int> node_ids;
                std::vector<std::string> dim_paths;
                std::vector<int> export_dim_idxs;
            };

            struct ProcessingMasks {
                std::vector<bool> value_node_mask;
                std::vector<bool> path_node_mask;
            };

            struct BufrTableInfo {
                std::vector<int> inode;
                std::vector<int> isc;
                std::vector<int> link;
                std::vector<int> itp;
                std::vector<std::string> typ;
                std::vector<std::string> tag;
                std::vector<int> jmpb;

                BufrTableInfo() = default;

                void update() const;
            };

            struct BufrData {
                std::vector<int> nval;
                std::vector<double> val;
                std::vector<std::vector<int>> inv;

                BufrData() = default;

                void update() const;
            };
        }  // namespace __details

    class Query
    {
     public:
        Query(const QuerySet& querySet);
        void query();

    private:
        const QuerySet querySet_;
        std::map<std::string, std::vector<__details::Target>> targetCache_;
        std::map<std::string, std::vector<__details::ProcessingMasks>> maskCache_;

        int fortranFileUnit_;
        int bufrLoc_;
        BufrDataManager bufrDataManager_;
        ResultSet resultSet_;

//        void findTargets(const std::string& subset,
//                         std::vector<Target>& targets,
//                         std::vector<ProcessingMasks>& masks) const;
//        Target findTarget(const std::string& subset, const std::string& targetName, const std::string query) const;
//        void collectData(const std::vector<Target>& targets,
//                         const std::vector<ProcessingMasks>& masks,
//                         ResultSet& resultSet) const;
//        void getDimInfo(const std::vector<int>& branches,
//                        int mnemonicCursor,
//                        std::vector<std::string>& dimPaths,
//                    // namespace Ingester      std::vector<int>& dimIdxs) const;
//        bool isQueryNode(int nodeIdx) const;
        };
    }  // namespace bufr
}  // namespace Ingester

