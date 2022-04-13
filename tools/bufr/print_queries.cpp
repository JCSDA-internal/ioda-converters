

#include <iostream>
#include <sstream>
#include <set>
#include <string>
#include <vector>
#include <algorithm>

#include "../../src/bufr/BufrParser/Query/DataProvider.h"
#include "../../src/bufr/BufrParser/Query/SubsetTable.h"

#include "bufr_interface.h"


static const char Esc = 27;


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


std::vector<Ingester::bufr::QueryData> getQueries(int fileUnit, const std::string& subset)
{
    static const int SubsetLen = 9;

    size_t msgNum = 0;

    int iddate;
    int bufrLoc;
    int il, im; // throw away
    char current_subset[9];

    std::vector<Ingester::bufr::QueryData> queryData;

    auto dataProvider = Ingester::bufr::DataProvider();
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
    std::cout << "Examples: " << std::endl;
    std::cout << "  ./print_queries.x ../data/bufr_satwnd_old_format.bufr" << std::endl;
    std::cout << "  ./print_queries.x -s NC005066 ../data/bufr_satwnd_old_format.bufr" << std::endl;
}


void printQueryList(const std::vector<Ingester::bufr::QueryData>& queries)
{
    for (auto query : queries)
    {
        std::ostringstream ostr;
        ostr << Esc << "[1m" << query.dimIdxs.size() << "d" << Esc << "[0m" << " ";
        ostr << Esc << "[1;34m" << query.pathComponents[0] << Esc << "[0m";
        for (size_t pathIdx = 1; pathIdx < query.pathComponents.size(); pathIdx++)
        {
            if (std::find(query.dimIdxs.begin(), query.dimIdxs.end(), pathIdx) != query.dimIdxs.end())
            {
                ostr << "/" << Esc << "[1;31m" << query.pathComponents[pathIdx] << Esc << "[0m";
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

        std::cout << ostr.str() << std::endl;
    }
}


void printQueries(const std::string& filePath, const std::string& subset, const std::string& tablePath)
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

    if (!subset.empty())
    {
        auto queries = getQueries(FileUnit, subset.c_str());
        std::cout << "Possible queries for subset: " << subset << std::endl;
        printQueryList(queries);
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
            std::cout << Esc << "[1;34m" << "  " << subset << Esc << "[0m" << std::endl;
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

            std::cout << "Possible queries for subset: "
                      << Esc << "[1;34m" << subset << Esc << "[0m"
                      << std::endl;
            printQueryList(getQueries(FileUnit, subset.c_str()));
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
        if (arg == "-s")
        {
            subset = std::string(argv[idx+1]);
            idx = idx + 2;
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


