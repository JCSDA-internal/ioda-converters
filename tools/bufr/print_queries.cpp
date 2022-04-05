

#include <iostream>
#include <sstream>
#include <set>
#include <string>
#include <vector>
#include <algorithm>

#include "../../src/bufr/BufrParser/Query/DataProvider.h"

#include "bufr_interface.h"


static const char Esc = 27;

struct Query
{
    std::vector<std::string> pathComponents;
    std::vector<size_t> dimIdxs;
};


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


std::vector<size_t> dimPathIdxs(const Ingester::bufr::DataProvider& dataProvider, std::vector<int> seqPath)
{
    std::vector<size_t> dimPathIdxs;
    dimPathIdxs.push_back(0);
    for (auto idx = 1; idx < seqPath.size(); idx++)
    {
        if (dataProvider.getTyp(seqPath[idx] - 1) == Ingester::bufr::Typ::DelayedRep ||
            dataProvider.getTyp(seqPath[idx] - 1) == Ingester::bufr::Typ::FixedRep ||
            dataProvider.getTyp(seqPath[idx] - 1) == Ingester::bufr::Typ::DelayedRepStacked)
        {
            dimPathIdxs.push_back(idx);
        }
    }

    return dimPathIdxs;
}


std::vector<std::string> makePathComponents(const Ingester::bufr::DataProvider& dataProvider,
                                            std::vector<int> seqPath,
                                            int nodeIdx)
{
    std::vector<std::string> pathComps;
    auto dimIdxs = dimPathIdxs(dataProvider, seqPath);

    pathComps.push_back(dataProvider.getTag(seqPath[0]));
    for (auto idx = 1; idx < seqPath.size(); idx++)
    {
        if (dataProvider.getTyp(seqPath[idx] - 1) == Ingester::bufr::Typ::DelayedRep ||
            dataProvider.getTyp(seqPath[idx] - 1) == Ingester::bufr::Typ::FixedRep ||
            dataProvider.getTyp(seqPath[idx] - 1) == Ingester::bufr::Typ::DelayedRepStacked ||
            dataProvider.getTyp(seqPath[idx] - 1) == Ingester::bufr::Typ::DelayedBinary)
        {
            pathComps.push_back(dataProvider.getTag(seqPath[idx]));
        }
    }

    pathComps.push_back(dataProvider.getTag(nodeIdx));
    return pathComps;
}

std::vector<Query> getQueries(int fileUnit, const std::string& subset)
{
    static const int SubsetLen = 9;

    size_t msgNum = 0;
    bool subsetFound = false;

    int iddate;
    int bufrLoc;
    int il, im; // throw away
    char current_subset[9];

    std::vector<Query> queries;

    auto dataProvider = Ingester::bufr::DataProvider();
    while (ireadmg_f(fileUnit, current_subset, &iddate, SubsetLen) == 0)
    {
        status_f(fileUnit, &bufrLoc, &il, &im);
        dataProvider.updateData(bufrLoc);

        msgNum++;
        if (std::string(current_subset) == subset)
        {
            subsetFound = true;
            std::vector<int> seqPath;

            seqPath.push_back(dataProvider.getInode());

            for (auto nodeIdx = dataProvider.getInode();
                 nodeIdx <= dataProvider.getIsc(dataProvider.getInode());
                 nodeIdx++)
            {
                if (dataProvider.getTyp(nodeIdx) == Ingester::bufr::Typ::Sequence ||
                    dataProvider.getTyp(nodeIdx) == Ingester::bufr::Typ::Repeat ||
                    dataProvider.getTyp(nodeIdx) == Ingester::bufr::Typ::StackedRepeat)
                {
                    seqPath.push_back(nodeIdx);
                }
                else if (dataProvider.getTyp(nodeIdx) == Ingester::bufr::Typ::Number ||
                         dataProvider.getTyp(nodeIdx) == Ingester::bufr::Typ::Character)
                {
                    auto query = Query();
                    query.pathComponents = makePathComponents(dataProvider, seqPath, nodeIdx);
                    query.dimIdxs = dimPathIdxs(dataProvider, seqPath);
                    queries.push_back(query);
                }

                if (seqPath.size() > 1)
                {
                    // Peak ahead to see if the next node is inside one of the containing sequences.
                    for (int pathIdx = seqPath.size() - 2; pathIdx >= 0; pathIdx--)
                    {
                        // Check if the node idx is the next node for the current path
                        // or if the parent node of the next node is the previous path index

                        if (seqPath[pathIdx] == dataProvider.getJmpb(nodeIdx + 1))
                        {
                            auto numToRewind = seqPath.size() - pathIdx - 1;
                            for (auto rewindIdx = 0; rewindIdx < numToRewind; rewindIdx++)
                            {
                                seqPath.pop_back();
                            }
                        }
                    }
                }
            }
        }

        if (subsetFound) break;
    }

    return queries;
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

void printQueryList(const std::vector<Query>& queries)
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


