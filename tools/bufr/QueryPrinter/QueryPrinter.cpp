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
            auto queries = getQueries({subset, 0});
            std::cout << subset << std::endl;
            std::cout << " Dimensioning Sub-paths: " << std::endl;
            printDimPaths(getDimPaths(*queries));
            std::cout << std::endl;
            std::cout << " Queries: " << std::endl;
            printQueryList(*queries);
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
                auto queries = getQueries(v);

                std::cout << v.str() << std::endl;
                std::cout << " Dimensioning Sub-paths: " << std::endl;
                printDimPaths(getDimPaths(*queries));
                std::cout << std::endl;
                std::cout << " Queries: " << std::endl;
                printQueryList(*queries);
                std::cout << std::endl;
            }
        }
    }


    std::vector<std::pair<int, std::string>>
    QueryPrinter::getDimPaths(const SubsetTable& table)
    {
        std::map<std::string, std::pair<int, std::string>> dimPathMap;
        for (const auto& leaf : table.getLeaves())
        {
            std::stringstream pathStream;
            pathStream << "*";
            for (size_t idx=1; idx <= static_cast<size_t>(leaf->getDimIdxs().back()); idx++)
            {
                pathStream << "/" << leaf->getPath()[idx];
            }

            dimPathMap[pathStream.str()] =
                std::make_pair(leaf->getDimIdxs().size(),
                               pathStream.str());
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


    void QueryPrinter::printQueryList(const SubsetTable& table)
    {
        for (auto leaf : table.getLeaves())
        {
            std::ostringstream ostr;
            ostr << dimStyledStr(static_cast<int>(leaf->getDimIdxs().size())) << "  ";
            ostr << typeStyledStr(leaf->typeInfo) << "  ";
            ostr << leaf->getPath()[0];
            for (size_t pathIdx = 1; pathIdx < leaf->getPath().size(); pathIdx++)
            {
                if (std::find(leaf->getDimIdxs().begin(),
                              leaf->getDimIdxs().end(),
                              pathIdx) != leaf->getDimIdxs().end())
                {
                    ostr << "/" << leaf->getPath()[pathIdx];
                }
                else
                {
                    ostr << "/" << leaf->getPath()[pathIdx];
                }
            }

            if (leaf->hasDuplicates)
            {
                ostr << "[" << leaf->copyIdx << "]";
            }

            std::cout << "  " << ostr.str() << std::endl;
        }
    }

}  // namespace bufr
}  // namespace Ingester