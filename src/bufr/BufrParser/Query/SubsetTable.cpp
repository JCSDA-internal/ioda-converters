/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "SubsetTable.h"

#include <algorithm>
#include <iostream>
#include <memory>
#include <sstream>


namespace Ingester {
namespace bufr {

    SubsetTable::SubsetTable(const DataProvider& dataProvider) :
      dataProvider_(dataProvider)
    {
        initialize();
    }

    std::vector<QueryData> SubsetTable::allQueryData()
    {
        std::vector<QueryData> queryData;

        for (const auto& leaf : root_->getLeaves())
        {
            QueryData data;
            data.isMissing = false;
            data.nodeId = leaf->nodeIdx;
            data.pathComponents = leaf->getPath();
            data.isString = leaf->type == Typ::Character;
            data.seqPath = leaf->getSeqPath();
            data.dimIdxs = leaf->getDimIdxs();
            data.requiresIdx = leaf->hasDuplicates;
            data.idx = leaf->copyIdx;

            queryData.push_back(data);
        }

        return queryData;
    }


    QueryData SubsetTable::dataForQuery(const std::vector<std::string>& queryComponents) const
    {
        QueryData queryData;
        auto key = mapKey(queryComponents);
        if (queryMap_.find(key) != queryMap_.end())
        {
            queryData = queryMap_.at(key);
        }
        else
        {
            queryData.isMissing = true;
            queryData.nodeId = 0;
            queryData.pathComponents = {};
            queryData.isString = false;
            queryData.seqPath = {};
            queryData.dimIdxs = {};
        }

        return queryData;
    }


    void SubsetTable::initialize()
    {
        root_ = std::make_shared<BufrNode>();
        root_->mnemonic = dataProvider_.getTag(dataProvider_.getInode());
        root_->type = Typ::Subset;
        root_->nodeIdx = dataProvider_.getInode();

        // Recursively parse the entire tree of BUFR nodes
        processNode(root_);
    }


    std::string SubsetTable::mapKey(const std::vector<std::string>& pathComponents,
                                    size_t idx) const
    {
        std::ostringstream ostr;
        for (size_t pathIdx = 1; pathIdx < pathComponents.size(); pathIdx++)
        {
            ostr << "/" << pathComponents[pathIdx];
        }

        if (idx > 0)
        {
            ostr << "[" << idx << "]";
        }

        return ostr.str();
    }

    std::string SubsetTable::mapKey(const std::shared_ptr<QueryData> query) const
    {
        std::string key;
        if (query->requiresIdx)
        {
            key = mapKey(query->pathComponents, query->idx);
        }
        else
        {
            key = mapKey(query->pathComponents);
        }

        return key;
    }

    void SubsetTable::processNode(std::shared_ptr<BufrNode>& parent)
    {
        if (!parent->isContainer())
        {
            return;
        }

        auto nodeIdx = static_cast<int> (parent->nodeIdx + 1);
        auto lastNode = static_cast<int>(dataProvider_.getLink(parent->nodeIdx) - 1);
        if (lastNode == -1) lastNode = dataProvider_.getIsc(dataProvider_.getInode());

        auto mnemonicCounts = std::unordered_map<std::string, size_t>();
        auto mnemonicMaps = std::unordered_map<std::string, std::vector<std::shared_ptr<BufrNode>>>();

        while (nodeIdx != 0 && nodeIdx <= lastNode)
        {
            parent->children.push_back(std::make_shared<BufrNode>());
            auto& newNode = parent->children.back();
            newNode->nodeIdx = nodeIdx;
            newNode->mnemonic = dataProvider_.getTag(nodeIdx);
            newNode->type = dataProvider_.getTyp(nodeIdx);
            newNode->parent = parent;

            // add to and increment duplicate mnemonic count, update duplicate status if there are duplicates
            const auto& mnemonic = newNode->mnemonic;
            if (mnemonicMaps.find(mnemonic) == mnemonicMaps.end())
            {
                mnemonicMaps[mnemonic] = std::vector<std::shared_ptr<BufrNode>>();
            }

            mnemonicMaps[mnemonic].push_back(newNode);
            newNode->copyIdx = mnemonicMaps[mnemonic].size();

            if (newNode->copyIdx > 1)
            {
                mnemonicMaps[mnemonic][0]->hasDuplicates = true;
                newNode->hasDuplicates = true;
            }

            processNode(newNode);
            nodeIdx = dataProvider_.getLink(nodeIdx);
        }
    }
}  // namespace bufr
}  // namespace Ingester
