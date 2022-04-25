

#include <iostream>
#include <sstream>
#include <set>
#include <string>
#include <vector>
#include <algorithm>

#include "../../src/bufr/BufrParser/Query/DataProvider.h"
#include "../../src/bufr/BufrParser/Query/SubsetTable.h"

#include "bufr_interface.h"


std::set<std::string> getSubsets(int fileUnit)
{
    static const int SubsetLen = 9;
    int iddate;

    std::set<std::string> subsets;

    char subset[SubsetLen];
    while (ireadmg_f(fileUnit, subset, &iddate, SubsetLen) == 0)
    {
        subsets.insert(std::string(subset));
    }

    return subsets;
}


std::vector<std::pair<int, std::string>>
getDimPaths(const std::vector<Ingester::bufr::QueryData>& queryData)
{
    std::map<std::string, std::pair<int, std::string>> dimPathMap;
    for (auto& query : queryData)
    {
        std::stringstream pathStream;
        pathStream << query.pathComponents[0];
        for (auto idx=1; idx <= query.dimIdxs[query.dimIdxs.size() - 1]; idx++)
        {
            pathStream << "/" << query.pathComponents[idx];
        }

        dimPathMap[pathStream.str()] =
                std::make_pair(query.dimIdxs.size(),
                               pathStream.str());
    }

    std::vector<std::pair<int, std::string>> result;
    for (auto& dimPath : dimPathMap)
    {
        result.push_back(dimPath.second);
    }

    return result;
}


std::vector<Ingester::bufr::QueryData> getQueries(int fileUnit,
                                                  const std::string& subset,
                                                  Ingester::bufr::DataProvider& dataProvider)
{
    static const int SubsetLen = 9;

    size_t msgNum = 0;

    int iddate;
    int bufrLoc;
    int il, im; // throw away
    char current_subset[9];

    std::vector<Ingester::bufr::QueryData> queryData;

    while (ireadmg_f(fileUnit, current_subset, &iddate, SubsetLen) == 0)
    {
        status_f(fileUnit, &bufrLoc, &il, &im);
        dataProvider.updateData(bufrLoc);

        msgNum++;
        if (std::string(current_subset) == subset)
        {
            queryData = Ingester::bufr::SubsetTable(dataProvider).allQueryData();
            break;
        }
    }

    return queryData;
}


void printHelp()
{
    std::cout << "Description: " << std::endl;
    std::cout << "  Lists all the queries possible on a BUFR file per subset." << std::endl;
    std::cout << "Arguments: " << std::endl;
    std::cout << "  -h          (Optional) Print out the help message." << std::endl;
    std::cout << "  -s <subset> (Optional) Print paths only for this subset." << std::endl;
    std::cout << "  input_file  Path to the BUFR file." << std::endl;
    std::cout << "  output_file  (Optional) Save the output. " << std::endl;
    std::cout << "Examples: " << std::endl;
    std::cout << "  ./print_queries.x ../data/bufr_satwnd_old_format.bufr" << std::endl;
    std::cout << "  ./print_queries.x -s NC005066 ../data/bufr_satwnd_old_format.bufr" << std::endl;
}

std::string dimStyledStr(int dims)
{
    std::ostringstream ostr;
    ostr << dims << "d";

    return ostr.str();
}

void printDimPaths(std::vector<std::pair<int, std::string>> dimPaths)
{
    for (auto& dimPath : dimPaths)
    {
        std::cout << "  " << dimPath.first << "d " << dimPath.second << std::endl;
    }
}

void printQueryList(const std::vector<Ingester::bufr::QueryData>& queries)
{
    for (auto query : queries)
    {
        std::ostringstream ostr;
        ostr << dimStyledStr(query.dimIdxs.size()) << " ";
        ostr << query.pathComponents[0];
        for (size_t pathIdx = 1; pathIdx < query.pathComponents.size(); pathIdx++)
        {
            if (std::find(query.dimIdxs.begin(), query.dimIdxs.end(), pathIdx) != query.dimIdxs.end())
            {
                ostr << "/" << query.pathComponents[pathIdx];
            }
            else
            {
                ostr << "/" << query.pathComponents[pathIdx];
            }
        }

        if (query.requiresIdx)
        {
            ostr << "[" << query.idx << "]";
        }

        std::cout << "  " << ostr.str() << std::endl;
    }
}

void printQueries(const std::string& filePath,
                  const std::string& subset,
                  const std::string& tablePath)
{
    const static int FileUnit = 12;
    const static int FileUnitTable1 = 13;
    const static int FileUnitTable2 = 14;

    open_f(FileUnit, filePath.c_str());

    if (tablePath.empty())
    {
        openbf_f(FileUnit, "IN", FileUnit);
    }
    else
    {
        openbf_f(FileUnit, "SEC3", FileUnit);
        mtinfo_f(tablePath.c_str(), FileUnitTable1, FileUnitTable2);
    }

    auto dataProvider = Ingester::bufr::DataProvider();
    if (!subset.empty())
    {
        auto queries = getQueries(FileUnit, subset.c_str(), dataProvider);
        std::cout << subset << std::endl;
        std::cout << " Dimensioning Sub-paths: " << std::endl;
        printDimPaths(getDimPaths(queries));
        std::cout << std::endl;
        std::cout << " Queries: " << std::endl;
        printQueryList(queries);
        std::cout << std::endl;
    }
    else
    {
        auto subsets = getSubsets(FileUnit);

        if (subsets.empty())
        {
            std::cerr << "No BUFR subsets found in " << filePath << std::endl;
            exit(1);
        }

        std::cout << "Available subsets: " << std::endl;
        for (auto subset : subsets)
        {
            std::cout << subset << std::endl;
        }
        std::cout << "Total number of subsets found: " << subsets.size() << std::endl << std::endl;


        for (auto subset : subsets)
        {
            closbf_f(FileUnit);
            close_f(FileUnit);

            open_f(FileUnit, filePath.c_str());

            if (tablePath.empty())
            {
                openbf_f(FileUnit, "IN", FileUnit);
            }
            else
            {
                openbf_f(FileUnit, "SEC3", FileUnit);
                mtinfo_f(tablePath.c_str(), FileUnitTable1, FileUnitTable2);
            }

            auto queries = getQueries(FileUnit, subset.c_str(), dataProvider);

            std::cout << subset << std::endl;
            std::cout << " Dimensioning Sub-paths: " << std::endl;
            printDimPaths(getDimPaths(queries));
            std::cout << std::endl;
            std::cout << " Queries: " << std::endl;
            printQueryList(queries);
            std::cout << std::endl;
        }
    }

    closbf_f(FileUnit);
    close_f(FileUnit);
}


int main(int argc, char** argv)
{
    std::string inputFile = "";
    std::string tablePath = "";
    std::string subset = "";

    int idx = 1;
    while (idx < argc)
    {
        std::string arg = argv[idx];
        if (arg.substr(0,2) == "-s")
        {
            if (arg.size() == 2)
            {
                subset = argv[idx+1];
                idx = idx + 2;
            }
            else
            {
                subset = arg.substr(2, arg.size());
                idx++;
            }
        }
        else if (arg == "-h")
        {
            printHelp();
            exit(0);
        }
        else if (arg == "-t")
        {
            tablePath = std::string(argv[idx + 1]);
            idx = idx + 2;
        }
        else
        {
            inputFile = arg;
            idx++;
        }
    }

    if (inputFile.empty())
    {
        printHelp();
        std::cerr << "Error: no input file specified" << std::endl;
        exit(1);
    }

    printQueries(inputFile, subset, tablePath);

    return 0;
}


