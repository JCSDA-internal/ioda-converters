/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "SubsetTable.h"

#include <algorithm>
#include <memory>

#include "QueryParser.h"


namespace Ingester {
namespace bufr {

    SubsetTable::SubsetTable(const DataProviderType& dataProvider) :
      dataProvider_(dataProvider)
    {
        initialize();
    }

    void SubsetTable::initialize()
    {
        root_ = std::make_shared<BufrNode>();
        root_->mnemonic = dataProvider_->getTag(dataProvider_->getInode());
        root_->type = Typ::Subset;
        root_->nodeIdx = dataProvider_->getInode();

        // Recursively parse the entire tree of BUFR nodes
        processNode(root_);

        // Find all the leaves of the tree
        leaves_ = root_->getLeaves();
    }

    void SubsetTable::processNode(std::shared_ptr<BufrNode>& parent)
    {
        if (!parent->isContainer())
        {
            return;
        }

        auto nodeIdx = static_cast<int> (parent->nodeIdx + 1);
        auto lastNode = static_cast<int>(dataProvider_->getLink(parent->nodeIdx) - 1);
        if (lastNode == -1) lastNode = dataProvider_->getIsc(dataProvider_->getInode());

        auto mnemonicCounts = std::unordered_map<std::string, size_t>();
        auto mnemonicMaps =
            std::unordered_map<std::string, std::vector<std::shared_ptr<BufrNode>>>();

        while (nodeIdx != 0 && nodeIdx <= lastNode)
        {
            parent->children.push_back(std::make_shared<BufrNode>());
            auto& newNode = parent->children.back();
            newNode->nodeIdx = nodeIdx;
            newNode->mnemonic = dataProvider_->getTag(nodeIdx);
            newNode->type = dataProvider_->getTyp(nodeIdx);
            newNode->parent = parent;

            // For hacky reasons we track the number of times we've seen a mnemonic
            // this is because NCEPLIB_bufr doeen't have a consept of unique nodes but relies
            // on mnemonics (which aren't unique) in an extremely bad way.
            if (mnemonicCnts_.find(newNode->mnemonic) == mnemonicCnts_.end())
            {
                mnemonicCnts_[newNode->mnemonic] = 1;
            }
            else
            {
                mnemonicCnts_[newNode->mnemonic]++;
            }
            newNode->mnemonicIdx = mnemonicCnts_[newNode->mnemonic];

            if (newNode->type == Typ::FixedRep)
            {
                newNode->fixedRepCount = dataProvider_->getIrf(nodeIdx);
            }
            else
            {
                newNode->fixedRepCount = 1;
            }

            // add to and increment duplicate mnemonic count, update duplicate status if there are
            // duplicates
            const auto& mnemonic = newNode->mnemonic;
            if (mnemonicMaps.find(mnemonic) == mnemonicMaps.end())
            {
                mnemonicMaps[mnemonic] = std::vector<std::shared_ptr<BufrNode>>();
            }

            mnemonicMaps[mnemonic].push_back(newNode);
            newNode->copyIdx = mnemonicMaps[mnemonic].size();

            if (parent->isQueryPathParentNode() && parent->hasDuplicates)
            {
                newNode->copyIdx = parent->copyIdx;
            }

            if (newNode->copyIdx > 1)
            {
                // We need to fix the initial nodes to indicate that they are duplicates
                mnemonicMaps[mnemonic][0]->hasDuplicates = true;
                if (mnemonicMaps[mnemonic][0]->isQueryPathParentNode())
                {
                    mnemonicMaps[mnemonic][0]->children[0]->hasDuplicates = true;
                }

                newNode->hasDuplicates = true;
            }

            processNode(newNode);

            if (newNode->isLeaf())
            {
                newNode->typeInfo = dataProvider_->getTypeInfo(newNode->nodeIdx);
            }

            nodeIdx = dataProvider_->getLink(nodeIdx);
        }
    }

    std::shared_ptr<BufrNode>
        SubsetTable::getNodeForPath(const std::vector<std::shared_ptr<PathComponent>>& path)
    {
        auto node = root_;
        for (const auto& component : path)
        {
            if (node->isContainer())
            {
                auto child = node->getChild(component->name, component->index);
                if (child)
                {
                    node = child;
                }
                else
                {
                    return nullptr;
                }
            }
            else
            {
                return nullptr;
            }
        }

        return node;
    }

}  // namespace bufr
}  // namespace Ingester
