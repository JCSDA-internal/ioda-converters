/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <array>
#include <unordered_map>

#include "QuerySet.h"
#include "ResultSet.h"
#include "DataProvider.h"
#include "Target.h"

namespace Ingester {
namespace bufr {
    namespace __details
    {
        /// \brief BUFR messages are indexed according to start and stop values that are dependant
        /// on the message itself (the indexing is a property of the message). This object allows
        /// lets you make an array where the indexing is offset with respect to the actual position
        /// of the object in the array.
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

        /// \brief Masks used to make the processing of BUFR data more efficient. The aim is to skip
        /// branches without data we care about.
        struct ProcessingMasks {
            std::vector<bool> valueNodeMask;
            std::vector<bool> pathNodeMask;
        };
    }  // namespace __details

    /// \brief Manages the execution of queries against on a BUFR file.
    class QueryRunner
    {
     public:
        /// \brief Constructor.
        /// \param[in] querySet The set of queries to execute against the BUFR file.
        /// \param[in, out] resultSet The object used to store the accumulated collected data.
        /// \param[in] dataProvider The BUFR data provider to use.
        QueryRunner(const QuerySet& querySet,
                    ResultSet& resultSet,
                    const DataProvider& dataProvider);
        void accumulate();

        Targets getTargets()
        {
            Targets targets;
            for (auto& subset : targetCache_)
            {
                for (auto& target : subset.second)
                {
                    targets.push_back(target);
                }
            }
            return targets;
        }

     private:
        const QuerySet querySet_;
        ResultSet& resultSet_;
        const DataProvider& dataProvider_;

        std::unordered_map<std::string, Targets> targetCache_;
        std::unordered_map<std::string, std::shared_ptr<__details::ProcessingMasks>> maskCache_;
        std::unordered_map<std::string, std::unordered_map<std::string, std::string>> unitCache_;


        /// \brief Look for the list of targets for the currently active BUFR message subset that
        /// apply to the QuerySet and cache them. Processing mask information is also collected in
        /// order to make the data collection more efficient.
        /// \param[in, out] targets The list of targets to populate.
        /// \param[in, out] masks The processing masks to populate.
        void findTargets(Targets& targets,
                         std::shared_ptr<__details::ProcessingMasks>& masks);


        /// \brief Find the target associated with a specific user provided query string.
        /// \param[in] targetName The name specified for the target.
        /// \param[in] query The query string to use.
        std::shared_ptr<Target> findTarget(const std::string &targetName,
                                           const Query& query) const;


        /// \brief Does the node idx correspond to an element you'd find in a query string (repeat
        /// or binary sequence)?
        /// \param[in] nodeIdx The node index to check.
        bool isQueryNode(int nodeIdx) const;


        /// \brief Get the dimensional information for the query with the given branches.
        /// \param[in] branches The branches to use.
        /// \param[in] mnemonicCursor The current position in the subset tree.
        /// \param[in, out] dimPaths The list of dimensioning query sub-paths
        /// \param[in, out] dimIdxs The idxs of the dimensioning elements in the query.
        void getDimInfo(const std::vector<int>& branches,
                        int mnemonicCursor,
                        std::vector<std::string>& dimPaths,
                        std::vector<int>& dimIdxs) const;


        /// \brief Accumulate the data for the currently open BUFR message subset.
        /// \param[in] targets The list of targets to collect for this subset.
        /// \param[in] masks The processing masks to use.
        /// \param[in, out] resultSet The object used to store the accumulated collected data.
        void collectData(Targets& targets,
                         std::shared_ptr<__details::ProcessingMasks> masks,
                         ResultSet& resultSet) const;
    };
}  // namespace bufr
}  // namespace Ingester
