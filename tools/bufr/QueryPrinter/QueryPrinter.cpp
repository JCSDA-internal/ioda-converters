/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "QueryPrinter.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <map>


namespace Ingester {
namespace bufr {

    QueryPrinter::QueryPrinter(DataProviderType dataProvider) :
        dataProvider_(dataProvider)
    {
    }

    void QueryPrinter::printQueries(const std::string &subset)
    {
        if (!subset.empty())
        {
            auto table = getTable({subset, 0});
            std::cout << subset << std::endl;
            std::cout << " Dimensioning Sub-paths: " << std::endl;
            printDimPaths(getDimPaths(table));
            std::cout << std::endl;
            std::cout << " Queries: " << std::endl;
            printQueryList(table);
            std::cout << std::endl;
        }
        else
        {
            dataProvider_->initAllTableData();
            auto variants = getSubsetVariants();

            if (variants.empty())
            {
                std::cerr << "No BUFR variants found in "
                          << dataProvider_->getFilepath() << std::endl;
                exit(1);
            }

            std::cout << "Available subset variants: " << std::endl;
            for (auto v : variants)
            {
                std::cout << v.str() << std::endl;
            }

            std::cout << "Total number of subset variants found: "
                      << variants.size()
                      << std::endl << std::endl;

            for (const auto& v : variants)
            {
                auto queries = getTable(v);

                std::cout << v.str() << std::endl;
                std::cout << " Dimensioning Sub-paths: " << std::endl;
                printDimPaths(getDimPaths(queries));
                std::cout << std::endl;
                std::cout << " Queries: " << std::endl;
                printQueryList(queries);
                std::cout << std::endl;
            }
        }
    }


    std::vector<std::pair<int, std::string>>
    QueryPrinter::getDimPaths(const SubsetTableType& table)
    {
        std::map<std::string, std::pair<int, std::string>> dimPathMap;
        for (const auto& leaf : table->getLeaves())
        {
            for (const auto& path : leaf->getDimPaths())
            {
                dimPathMap[path] = std::make_pair(leaf->getDimIdxs().size(), path);
            }
        }

        std::vector<std::pair<int, std::string>> result;
        for (auto& dimPath : dimPathMap)
        {
            result.push_back(dimPath.second);
        }

        return result;
    }


    std::string QueryPrinter::dimStyledStr(int dims)
    {
        std::ostringstream ostr;
        ostr << dims << "d";

        return ostr.str();
    }


    std::string QueryPrinter::typeStyledStr(const TypeInfo& info)
    {
        std::string typeStr;

        if (info.isString())
        {
            typeStr = "string";
        }
        else if (info.isInteger())
        {
            if (info.isSigned())
            {
                if (info.is64Bit())
                {
                    typeStr = "int64 ";
                }
                else
                {
                    typeStr = "int   ";
                }
            }
            else
            {
                if (info.is64Bit())
                {
                    typeStr = "uint64";
                }
                else
                {
                    typeStr = "uint  ";
                }
            }
        }
        else
        {
            if (info.is64Bit())
            {
                typeStr = "double";
            }
            else
            {
                typeStr = "float ";
            }
        }

        return typeStr;
    }


    void QueryPrinter::printDimPaths(std::vector<std::pair<int, std::string>> dimPaths)
    {
        for (auto& dimPath : dimPaths)
        {
            std::cout << "  " << dimPath.first << "d  " << dimPath.second << std::endl;
        }
    }


    void QueryPrinter::printQueryList(const SubsetTableType& table)
    {
        for (auto leaf : table->getLeaves())
        {
            std::vector<std::string> pathComponents;
            std::shared_ptr<BufrNode> currentNode = leaf;

            while (currentNode != nullptr)
            {
                if (currentNode->isQueryPathNode() || currentNode->isLeaf())
                {
                    std::ostringstream pathStr;
                    pathStr << currentNode->mnemonic;

                    if (currentNode->hasDuplicates)
                    {
                        pathStr << "[" << currentNode->copyIdx << "]";
                    }

                    pathComponents.push_back(pathStr.str());
                }

                currentNode = currentNode->parent.lock();
            }

            std::ostringstream ostr;
            ostr << dimStyledStr(static_cast<int>(leaf->getDimIdxs().size())) << "  ";
            ostr << typeStyledStr(leaf->typeInfo) << "  ";

            for (auto it = pathComponents.rbegin(); it != pathComponents.rend(); ++it)
            {
                if (it != pathComponents.rbegin()) ostr << "/";
                ostr << *it;
            }

            std::cout << "  " << ostr.str() << std::endl;
        }
    }

}  // namespace bufr
}  // namespace Ingester