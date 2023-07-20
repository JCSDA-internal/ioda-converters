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
#include <iostream>
#include <sstream>

#include "DataProvider/DataProvider.h"

namespace Ingester {
namespace bufr {

    struct PathComponent;

    /// \brief A node in the bufr data tree (metadata). This is a recursive data structure.
    struct BufrNode : public std::enable_shared_from_this<BufrNode>
    {
        std::weak_ptr<BufrNode> parent;
        std::string mnemonic;
        Typ type;
        std::vector<std::shared_ptr<BufrNode>> children;
        size_t nodeIdx;
        size_t copyIdx;
        size_t mnemonicIdx;
        bool hasDuplicates;
        TypeInfo typeInfo;
        size_t fixedRepCount;

        /// \brief Do this nodes child sequences appear as parts of the query string?
        /// \return True if this node is a parent of a query path node.
        bool isQueryPathParentNode()
        {
            return type == Typ::DelayedRep ||
                   type == Typ::FixedRep ||
                   type == Typ::DelayedRepStacked ||
                   type == Typ::DelayedBinary;
        }

        /// \brief Does this node appear as part of the query string?
        /// \return True if this node is a query path node.
        bool isQueryPathNode()
        {
            bool isQueryPath = true;
            if (!parent.expired())
            {
                isQueryPath = parent.lock()->isQueryPathParentNode();
            }

            return isQueryPath;
        }

        /// \brief Does this node add dimension to the resulting data? (i.e. is it a repeat node)
        /// \return True if this node is a dimensioning node.
        bool isDimensioningNode()
        {
            return type == Typ::DelayedRep ||
                   type == Typ::FixedRep ||
                   type == Typ::DelayedRepStacked ||
                   type == Typ::Subset;
        }

        /// \brief Is this node a leaf node (i.e. a data node)
        /// \return True if this node is a leaf node.
        bool isLeaf() const
        {
            return type == Typ::Number ||
                   type == Typ::Character;
        }

        /// \brief Does this type of node contain children?
        /// \return True if this node is a container node.
        bool isContainer() const
        {
            return type == Typ::DelayedRep ||
                   type == Typ::DelayedRepStacked ||
                   type == Typ::DelayedBinary ||
                   type == Typ::FixedRep ||
                   type == Typ::Repeat ||
                   type == Typ::StackedRepeat ||
                   type == Typ::Sequence ||
                   type == Typ::Subset;
        }

        /// \brief Recursively get the path to this node as a vector of strings.
        /// \return The path to this node as a vector of strings.
        std::vector<std::string> getPath()
        {
            std::vector<std::string> components;
            return getPath(components);
        }

        /// \brief Recursively get the path to this node as a vector of strings.
        /// \param components The path to this node as a vector of strings.
        /// \return The path to this node as a vector of strings.
        std::vector<std::string> getPath(std::vector<std::string>& components)
        {
            if (!parent.expired())
            {
                components = parent.lock()->getPath(components);
            }

            if (isQueryPathNode() || isLeaf())
            {
                components.push_back(mnemonic);
            }

            return components;
        }

        /// \brief Recursively get the path to this node as a vector of BufrNodes.
        /// \return The path to this node as a vector of BufrNodes.
        std::vector<std::shared_ptr<BufrNode>> getPathNodes()
        {
            std::vector<std::shared_ptr<BufrNode>> components;
            return getPathNodes(components);
        }

        /// \brief Recursively get the path to this node as a vector of BufrNodes.
        /// \param components The path to this node as a vector of BufrNodes.
        /// \return The path to this node as a vector of BufrNodes.
        std::vector<std::shared_ptr<BufrNode>>
            getPathNodes(std::vector<std::shared_ptr<BufrNode>>& components)
        {
            if (!parent.expired())
            {
                components = parent.lock()->getPathNodes(components);
            }

            if (isQueryPathNode())
            {
                components.push_back(parent.lock());
            }
            else if (isLeaf())
            {
                components.push_back(shared_from_this());
            }

            return components;
        }

        /// \brief Get the sub paths of this node that map to a dimension.
        /// \return The sub paths of this node that map to a dimension.
        std::vector<std::string> getDimPaths()
        {
            std::vector<std::string> components;
            getDimPaths(components);
            return components;
        }

        /// \brief Recursively get the sub paths of this node that map to a dimension.
        /// \param components The sub paths of this node that map to a dimension.
        void getDimPaths(std::vector<std::string>& components)
        {
            if (!parent.expired())
            {
                parent.lock()->getDimPaths(components);
            }

            if (parent.expired())
            {
                components.push_back("*");
            }
            else if (parent.lock()->isDimensioningNode() && isQueryPathNode())
            {
                std::ostringstream pathSubStr;
                auto subPath = getPath();
                for (auto it = subPath.begin(); it != subPath.end(); ++it)
                {
                    if (it == subPath.begin())
                    {
                        pathSubStr << "*";  // Always insert * for the subset
                    }
                    else
                    {
                        pathSubStr << "/" << *it;
                    }
                }

                components.push_back(pathSubStr.str());
            }
        }

        /// \brief Get the nodes that are the leaves of the tree.
        /// \return The nodes that are the leaves of the tree.
        std::vector<std::shared_ptr<BufrNode>> getLeaves()
        {
            std::vector<std::shared_ptr<BufrNode>> leaves;
            getLeaves(leaves);
            return leaves;
        }

        /// \brief Recursively get the nodes that are the leaves of the tree.
        /// \param leaves The nodes that are the leaves of the tree.
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

        /// \brief Get the indices of the nodes that add dimensions to the data.
        /// \return The indices of the nodes that add dimensions to the data.
        std::vector<int> getDimIdxs()
        {
            std::vector<int> idxs;
            int depth = 0;
            return getDimIdxs(idxs, depth);
        }

        /// \brief Recursively get the indices of the nodes that add dimensions to the data.
        /// \param idxs The indices of the nodes that add dimensions to the data.
        /// \param depth The depth of this node in the tree.
        std::vector<int> getDimIdxs(std::vector<int>& idxs, int& depth)
        {
            if (!parent.expired())
            {
                idxs = parent.lock()->getDimIdxs(idxs, depth);
            }

            if (isDimensioningNode())
            {
                idxs.push_back(depth);
            }

            if (isQueryPathNode())
            {
                depth++;
            }

            return idxs;
        }

        /// \brief Get the children of this node.
        /// \return The children of this node.
        std::shared_ptr<BufrNode> getChild(const std::string& mnemonic, size_t index)
        {
            size_t currentIdx = 0;
            for (const auto& child : children)
            {
                if (child->mnemonic == mnemonic)
                {
                    currentIdx++;
                    if (currentIdx == index || index == 0) return child;
                }
                else if (child->isQueryPathParentNode() && child->children[0]->mnemonic == mnemonic)
                {
                    currentIdx++;
                    if (currentIdx == index || index == 0) return child->children[0];
                }
                else if (child->isContainer() && !child->isQueryPathNode())
                {
                    auto node = child->getChild(mnemonic, index);
                    if (node != nullptr) return node;
                }
            }

            return nullptr;
        }

        /// \brief Get the dimensioning parent of this node.
        /// \return The dimensioning parent of this node.
        std::shared_ptr<BufrNode> getParent()
        {
            if (!parent.expired())
            {
                return parent.lock();
            }

            return nullptr;
        }

        /// \brief Get the dimensioning parent of this node.
        /// \return The dimensioning parent of this node.
        std::shared_ptr<BufrNode> getDimensionParent()
        {
            if (!parent.expired())
            {
                if (parent.lock()->isDimensioningNode())
                {
                    return parent.lock();
                }
                else
                {
                    return parent.lock()->getDimensionParent();
                }
            }

            return nullptr;
        }
    };

    typedef std::vector<std::shared_ptr<Ingester::bufr::BufrNode>> BufrNodeVector;


    /// \brief Parses the BUFR message subset Meta data tables.
    class SubsetTable
    {
     public:
        SubsetTable() = delete;
        explicit SubsetTable(const DataProviderType& dataProvider);
        ~SubsetTable() = default;

        /// \brief Returns all the leaf data elements in the subset table.
        /// \returns A vector of BufrNode objects.
        const BufrNodeVector& getLeaves() const { return leaves_; }

        const std::shared_ptr<BufrNode> getRoot() const { return root_; }

        /// \brief Gets the node for the path that is passed in.
        /// \param path The path to the node.
        /// \returns A shared pointer to the node.
        std::shared_ptr<BufrNode>
            getNodeForPath(const std::vector<std::shared_ptr<PathComponent>>& path);

        std::string getLongStrId(FortranIdx idx) const;

    private:
        const DataProviderType dataProvider_;
        std::shared_ptr<BufrNode> root_;
        BufrNodeVector leaves_;
        std::unordered_map<size_t, std::shared_ptr<BufrNode>> nodeIdxMap_;
        std::unordered_map<std::string, size_t> mnemonicCnts_;

        /// \brief Initializes the subset table.
        void initialize();

        /// \brief Parses the BUFR message subset Meta data tables in a recursive ve fashion.
        /// \param[in] parent The current parent node in the tree.
        void processNode(std::shared_ptr<BufrNode>& parent);
    };

    typedef std::shared_ptr<SubsetTable> SubsetTableType;
}  // namespace bufr
}  // namespace Ingester

