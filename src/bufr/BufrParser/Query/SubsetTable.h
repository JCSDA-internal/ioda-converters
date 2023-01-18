/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <vector>
#include <string>
#include <unordered_map>
#include <set>

#include "DataProvider.h"

namespace Ingester {
namespace bufr {

    /// \brief Meta data for a query according to the BUFR subset table.
    struct QueryData
    {
        bool isMissing;
        int nodeId;
        std::vector<std::string> pathComponents;
        bool isString;
        std::vector<size_t> seqPath;
        std::vector<size_t> dimIdxs;
        size_t idx;
        bool requiresIdx;
        TypeInfo typeInfo;
    };

    struct BufrNode : public std::enable_shared_from_this<BufrNode>
    {
        std::weak_ptr<BufrNode> parent;
        std::string mnemonic;
        Typ type;
        std::vector<std::shared_ptr<BufrNode>> children;
        size_t nodeIdx;
        size_t copyIdx;
        bool hasDuplicates;

        // \brief Do this nodes child sequences appear as parts of the query string?
        bool isQueryPathParentNode()
        {
            return type == Typ::DelayedRep ||
                   type == Typ::FixedRep ||
                   type == Typ::DelayedRepStacked ||
                   type == Typ::DelayedBinary ||
                   type == Typ::StackedRepeat;
        }

        bool isQueryPathNode()
        {
            bool isQueryPath = true;
            if (parent.lock() != nullptr)
            {
                isQueryPath = parent.lock()->isQueryPathParentNode();
            }

            return isQueryPath;
        }

        // \brief Does this node add dimension to the resulting data? (i.e. is it a repeat node)
        bool isDimensioningNode()
        {
            return type == Typ::DelayedRep ||
                   type == Typ::FixedRep ||
                   type == Typ::DelayedRepStacked ||
                   type == Typ::StackedRepeat ||
                   type == Typ::Subset;
        }

        // \brief Is this node a leaf node (i.e. a data node)
        bool isLeaf() const
        {
            return type == Typ::Number ||
                   type == Typ::Character;
        }

        // \brief Does this type of node contain children?
        bool isContainer() const
        {
            return type == Typ::DelayedRep ||
                   type == Typ::DelayedRepStacked ||
                   type == Typ::DelayedBinary ||
                   type == Typ::FixedRep ||
                   type == Typ::Repeat ||
                   type == Typ::Sequence ||
                   type == Typ::Subset;
        }

        std::vector<std::string> getPath()
        {
            std::vector<std::string> components;
            return getPath(components);
        }

        std::vector<std::string> getPath(std::vector<std::string>& components)
        {
            if (parent.lock() != nullptr)
            {
                components = parent.lock()->getPath(components);
            }

            if (isQueryPathNode() || isLeaf())
            {
                components.push_back(mnemonic);
            }

            return components;
        }

        std::vector<std::shared_ptr<BufrNode>> getLeaves()
        {
            std::vector<std::shared_ptr<BufrNode>> leaves;
            getLeaves(leaves);
            return leaves;
        }

        void getLeaves(std::vector<std::shared_ptr<BufrNode>>& leaves)
        {
            if (isLeaf())
            {
                leaves.push_back(shared_from_this());
            }
            else
            {
                for (auto& child : children)
                {
                    child->getLeaves(leaves);
                }
            }
        }


        std::vector<size_t> getSeqPath()
        {
            std::vector<size_t> path;
            return getSeqPath(path);
        }

        std::vector<size_t> getSeqPath(std::vector<size_t>& path)
        {
            if (parent.lock() != nullptr)
            {
                path = parent.lock()->getSeqPath(path);
            }

            path.push_back(nodeIdx);

            return path;
        }


        std::vector<size_t> getDimIdxs()
        {
            std::vector<size_t> idxs;
            size_t depth = 0;
            return getDimIdxs(idxs, depth);
        }

        std::vector<size_t> getDimIdxs(std::vector<size_t>& idxs, size_t& depth)
        {
            if (parent.lock() != nullptr)
            {
                idxs = parent.lock()->getDimIdxs(idxs, depth);
            }

            if (isDimensioningNode())
            {
                idxs.push_back(depth);
                depth++;
            }

            return idxs;
        }
    };


    /// \brief Parses the BUFR message subset Meta data tables.
    class SubsetTable
    {
     public:
        SubsetTable() = delete;
        explicit SubsetTable(const DataProvider& dataProvider);
        ~SubsetTable() = default;

        /// \brief Returns all the query data elements.
        /// \returns BUFR table meta data for all the queries
        std::vector<QueryData> allQueryData();

        /// \brief Get meta data for the query with the given components.
        QueryData dataForQuery(const std::vector<std::string>& queryComponents) const;

     private:
        const DataProvider& dataProvider_;
        std::shared_ptr<BufrNode> root_;
        std::unordered_map<std::string, QueryData> queryMap_;
        std::vector<std::string> queryMapKeys_;

        void initialize();
        std::vector<size_t> dimPathIdxs(std::vector<size_t> seqPath) const;
        std::vector<std::string> makePathComponents(std::vector<size_t> seqPath, int nodeIdx);
        std::string mapKey(const std::vector<std::string>& pathComponents, size_t idx = 0) const;
        std::string mapKey(const std::shared_ptr<QueryData> query) const;

       void processNode(std::shared_ptr<BufrNode>& parent);

//        void parseSequence(std::shared_ptr<Node> parentNode, size_t& nodeIdx, size_t endIdx);
    };
}  // namespace bufr
}  // namespace Ingester

