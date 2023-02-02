
#include <algorithm>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>


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
        auto str_subset = std::string(subset);
        str_subset.erase(
            remove_if(str_subset.begin(), str_subset.end(), isspace), str_subset.end());
        subsets.insert(str_subset);
    }

    return subsets;
}

std::vector<std::pair<int, std::string>>
getDimPaths(const Ingester::bufr::BufrNodeVector& leaves)
{
    std::map<std::string, std::pair<int, std::string>> dimPathMap;
    for (const auto& leaf : leaves)
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


std::shared_ptr<Ingester::bufr::SubsetTable> getTable(int fileUnit,
                                                      const std::string& subset,
                                                      Ingester::bufr::DataProvider& dataProvider)
{
    static const int SubsetLen = 9;

    int iddate;
    int bufrLoc;
    int il, im; // throw away
    char current_subset[9];
    bool subsetFound = false;

    std::shared_ptr<Ingester::bufr::SubsetTable> table = nullptr;

    while (ireadmg_f(fileUnit, current_subset, &iddate, SubsetLen) == 0)
    {
        auto msg_subset = std::string(current_subset);
        msg_subset.erase(
                remove_if(msg_subset.begin(), msg_subset.end(), isspace), msg_subset.end());

        status_f(fileUnit, &bufrLoc, &il, &im);
        dataProvider.updateData(bufrLoc);

        if (msg_subset == subset)
        {
            while (ireadsb_f(fileUnit) == 0)
            {
                status_f(fileUnit, &bufrLoc, &il, &im);
                dataProvider.updateData(bufrLoc);
                table = std::make_shared<Ingester::bufr::SubsetTable> (dataProvider);
                subsetFound = true;
            }
        }

        if (subsetFound) break;
    }

    return table;
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
        std::cout << "  " << dimPath.first << "d  " << dimPath.second << std::endl;
    }
}

void printQueryList(const Ingester::bufr::BufrNodeVector& leaves)
{
    for (auto leaf : leaves)
    {
        std::ostringstream ostr;
        ostr << dimStyledStr(leaf->getDimIdxs().size()) << "  ";
        ostr << leaf->typeInfo.str() << "  ";
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

    auto dataProvider = Ingester::bufr::DataProvider(FileUnit);
    if (!subset.empty())
    {
        auto table = getTable(FileUnit, subset.c_str(), dataProvider);
        std::cout << subset << std::endl;
        std::cout << " Dimensioning Sub-paths: " << std::endl;
        printDimPaths(getDimPaths(table->getLeaves()));
        std::cout << std::endl;
        std::cout << " Queries: " << std::endl;
        printQueryList(table->getLeaves());
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

            auto table = getTable(FileUnit, subset.c_str(), dataProvider);

            std::cout << subset << std::endl;
            std::cout << " Dimensioning Sub-paths: " << std::endl;
            printDimPaths(getDimPaths(table->getLeaves()));
            std::cout << std::endl;
            std::cout << " Queries: " << std::endl;
            printQueryList(table->getLeaves());
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

    try
    {
        printQueries(inputFile, subset, tablePath);
    }
    catch (const std::exception &e)
    {
        throw;
    }

    return 0;
}


