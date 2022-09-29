/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "IodaEncoder.h"

#include <memory>
#include <map>
#include <string>
#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "oops/util/Logger.h"

#include "ioda/Layout.h"
#include "ioda/Misc/DimensionScales.h"

#include <boost/algorithm/string.hpp>


namespace Ingester
{
    static const char* LocationName = "Location";
    static const char* DefualtDimName = "dim";

    IodaEncoder::IodaEncoder(const eckit::Configuration& conf):
        description_(IodaDescription(conf))
    {
    }

    std::map<SubCategory, ioda::ObsGroup>
        IodaEncoder::encode(const std::shared_ptr<DataContainer>& dataContainer, bool append)
    {
        auto backendParams = ioda::Engines::BackendCreationParameters();
        std::map<SubCategory, ioda::ObsGroup> obsGroups;

        // Get the named dimensions
        NamedPathDims namedLocDims;
        NamedPathDims namedExtraDims;

        {
            std::set<std::string> dimNames;
            std::set<std::string> dimPaths;
            for (const auto& dim : description_.getDims())
            {
                if (dimNames.find(dim.name) != dimNames.end())
                {
                    throw eckit::UserError("ioda::dimensions: Duplicate dimension name: "
                          + dim.name);
                }

                dimNames.insert(dim.name);

                for (auto path : dim.paths)
                {
                    if (dimPaths.find(path) != dimPaths.end())
                    {
                        throw eckit::BadParameter("ioda::dimensions: Declared duplicate dim. path: "
                              + path);
                    }

                    if (path.substr(0, 1) != "*")
                    {
                        std::ostringstream errStr;
                        errStr << "ioda::dimensions: ";
                        errStr << "Path " << path << " must start with *. ";
                        errStr << "Subset specific named dimensions are not supported.";

                        throw eckit::BadParameter(errStr.str());
                    }

                    dimPaths.insert(path);
                }

                namedExtraDims.insert({dim.paths, dim.name});
            }
        }

        for (const auto& categories : dataContainer->allSubCategories())
        {
            // Create the dimensions variables
            std::map<std::string, std::shared_ptr<ioda::NewDimensionScale_Base>> dimMap;

            auto dataObjectGroupBy = dataContainer->getGroupByObject(
                description_.getVariables()[0].source, categories);

            dimMap[LocationName] = ioda::NewDimensionScale<int>(
                LocationName, dataObjectGroupBy->getDims()[0]);

            namedLocDims[{dataObjectGroupBy->getDimPaths()[0]}] = LocationName;

            int autoGenDimNumber = 2;
            for (const auto& varDesc : description_.getVariables())
            {
                auto dataObject = dataContainer->get(varDesc.source, categories);

                for (std::size_t dimIdx  = 1; dimIdx < dataObject->getDimPaths().size(); dimIdx++)
                {
                    auto dimPath = dataObject->getDimPaths()[dimIdx];
                    std::string dimName = "";

                    if (existsInNamedPath(dimPath, namedExtraDims))
                    {
                        dimName = nameForDimPath(dimPath, namedExtraDims);
                    }
                    else
                    {
                        auto newDimStr = std::ostringstream();
                        newDimStr << DefualtDimName << "_" << autoGenDimNumber;

                        dimName = newDimStr.str();
                        namedExtraDims[{dimPath}] = dimName;
                        autoGenDimNumber++;
                    }

                    if (dimMap.find(dimName) == dimMap.end())
                    {
                        dimMap[dimName] = ioda::NewDimensionScale<int>(dimName,
                                          dataObject->getDims()[dimIdx]);
                    }
                }
            }

            // When we find that the primary index is zero we need to skip this category
            if (dataObjectGroupBy->getDims()[0] == 0)
            {
                for (auto category : categories)
                {
                    oops::Log::warning() << "  Skipped category " << category << std::endl;
                }

                continue;
            }

            // Make the filename string
            if (description_.getBackend() == ioda::Engines::BackendNames::Hdf5File)
            {
                std::string filename = description_.getFilepath();

                size_t catIdx = 0;
                std::map<std::string, std::string> substitutions;
                for (const auto &catPair : dataContainer->getCategoryMap())
                {
                    substitutions.insert({catPair.first, categories.at(catIdx)});
                    catIdx++;
                }

                backendParams.fileName = makeStrWithSubstitions(filename, substitutions);
            }

            backendParams.openMode = ioda::Engines::BackendOpenModes::Read_Write;
            backendParams.createMode = ioda::Engines::BackendCreateModes::Truncate_If_Exists;
            backendParams.action = append ? ioda::Engines::BackendFileActions::Open : \
                                        ioda::Engines::BackendFileActions::Create;
            backendParams.flush = true;
            backendParams.allocBytes = dataContainer->size(categories);

            auto rootGroup = ioda::Engines::constructBackend(description_.getBackend(),
                                                             backendParams);

            ioda::NewDimensionScales_t allDims;
            for (auto dimPair : dimMap)
            {
                allDims.push_back(dimPair.second);
            }

            auto policy = ioda::detail::DataLayoutPolicy::Policies::ObsGroup;
            auto layoutPolicy = ioda::detail::DataLayoutPolicy::generate(policy);
            auto obsGroup = ioda::ObsGroup::generate(rootGroup, allDims, layoutPolicy);

            // Create Globals
            for (auto& global : description_.getGlobals())
            {
                global->addTo(rootGroup);
            }

            // Create Variables
            for (const auto& varDesc : description_.getVariables())
            {
                std::vector<ioda::Dimensions_t> chunks;
                auto dimensions = std::vector<ioda::Variable>();
                auto data = dataContainer->get(varDesc.source, categories);
                for (size_t dimIdx = 0; dimIdx < data->getDims().size(); dimIdx++)
                {
                    auto dimPath = data->getDimPaths()[dimIdx];

                    NamedPathDims namedPathDims;
                    if (dimIdx == 0)
                    {
                        namedPathDims = namedLocDims;
                    }
                    else
                    {
                        namedPathDims = namedExtraDims;
                    }

                    auto dimVar = obsGroup.vars[nameForDimPath(dimPath, namedPathDims)];
                    dimensions.push_back(dimVar);

                    if (dimIdx < varDesc.chunks.size())
                    {
                        chunks.push_back(std::min(dimVar.getChunkSizes()[0],
                                                  varDesc.chunks[dimIdx]));
                    }
                    else
                    {
                        chunks.push_back(dimVar.getChunkSizes()[0]);
                    }
                }

                auto var = data->createVariable(obsGroup,
                                                varDesc.name,
                                                dimensions,
                                                chunks,
                                                varDesc.compressionLevel);

                var.atts.add<std::string>("long_name", { varDesc.longName }, {1});

                if (!varDesc.units.empty())
                {
                    var.atts.add<std::string>("units", { varDesc.units }, {1});
                }

                if (varDesc.coordinates)
                {
                    var.atts.add<std::string>("coordinates", { *varDesc.coordinates }, {1});
                }

                if (varDesc.range)
                {
                    var.atts.add<float>("valid_range",
                                            {varDesc.range->start, varDesc.range->end},
                                            {2});
                }
            }

            obsGroups.insert({categories, obsGroup});
        }

        return obsGroups;
    }

    std::string IodaEncoder::makeStrWithSubstitions(const std::string& prototype,
                                                   const std::map<std::string, std::string>& subMap)
    {
        auto resultStr = prototype;
        auto subIdxs = findSubIdxs(prototype);

        std::reverse(subIdxs.begin(), subIdxs.end());

        for (const auto& subs : subIdxs)
        {
            if (subMap.find(subs.first) != subMap.end())
            {
                auto repIdxs = subs.second;
                resultStr = resultStr.replace(repIdxs.first,
                                              repIdxs.second - repIdxs.first + 1,
                                              subMap.at(subs.first));
            }
            else
            {
                std::ostringstream errStr;
                errStr << "Can't find " <<  subs.first << ". No category with that name.";
                throw eckit::BadParameter(errStr.str());
            }
        }

        return resultStr;
    }

    std::vector<std::pair<std::string, std::pair<int, int>>>
    IodaEncoder::findSubIdxs(const std::string& str)
    {
        std::vector<std::pair<std::string, std::pair<int, int>>>  result;

        size_t startPos = 0;
        size_t endPos = 0;

        while (startPos < str.size())
        {
            startPos = str.find("{", startPos+1);

            if (startPos < str.size())
            {
                endPos = str.find("}", startPos+1);

                if (endPos < str.size())
                {
                    result.push_back({str.substr(startPos + 1, endPos - startPos - 1),
                                      {startPos, endPos}});
                    startPos = endPos;
                }
                else
                {
                    throw eckit::BadParameter("Unmatched { found in output filename.");
                }
            }
        }

        return result;
    }

    bool IodaEncoder::existsInNamedPath(const std::string& path, const NamedPathDims& pathMap) const
    {
        for (auto paths : pathMap)
        {
            if (std::find(paths.first.begin(), paths.first.end(), path) != paths.first.end())
            {
                return true;
            }
        }

        return false;
    }

    std::string IodaEncoder::nameForDimPath(const std::string& path,
                const NamedPathDims& pathMap) const
    {
        std::string name;

        for (auto paths : pathMap)
        {
            if (std::find(paths.first.begin(), paths.first.end(), path) != paths.first.end())
            {
                name = paths.second;
                break;
            }
        }

        return name;
    }
}  // namespace Ingester
