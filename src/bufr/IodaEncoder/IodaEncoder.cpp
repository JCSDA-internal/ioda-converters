/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "IodaEncoder.h"

#include <memory>
#include <type_traits>
#include <map>
#include <string>
#include <sstream>

#include "eckit/exception/Exceptions.h"

#include "ioda/Layout.h"
#include "ioda/Misc/DimensionScales.h"

#include <boost/algorithm/string.hpp>


namespace Ingester
{
    IodaEncoder::IodaEncoder(const eckit::Configuration& conf) :
        description_(IodaDescription(conf))
    {
    }

    IodaEncoder::IodaEncoder(const IodaDescription& description) :
        description_(description)
    {
    }

    std::map<SubCategory, ioda::ObsGroup>
        IodaEncoder::encode(const std::shared_ptr<DataContainer>& dataContainer, bool append)
    {
        auto backendParams = ioda::Engines::BackendCreationParameters();
        std::map<SubCategory, ioda::ObsGroup> obsGroups;

        // Get the named dimensions
        std::map<std::string, std::string> namedPathDims;

        {
            std::set<std::string> dimNames;
            std::set<std::string> dimPaths;
            for (const auto& dim : description_.getDims())
            {
                if (dimNames.find(dim.name) != dimNames.end())
                {
                    throw eckit::UserError("Duplicate dimension name: " + dim.name);
                }
                if (dimPaths.find(dim.path) != dimPaths.end())
                {
                    throw eckit::UserError("Duplicate dimension path: " + dim.path);
                }

                namedPathDims.insert({dim.path, dim.name});
                dimNames.insert(dim.name);
                dimPaths.insert(dim.path);
            }
        }

        for (const auto& categories : dataContainer->allSubCategories())
        {
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

            // Create the dimensions variables
            std::map<std::string, std::shared_ptr<ioda::NewDimensionScale_Base>> knownDims;
            knownDims.insert({"*", ioda::NewDimensionScale<int>("nlocs", dataContainer->size(categories))});
            namedPathDims.insert({"*", "nlocs"});

            int autoGenDimNumber = 1;
            for (const auto& varDesc : description_.getVariables())
            {
                auto dataObject = dataContainer->get(varDesc.source, categories);

                for (auto dimIdx  = 0; dimIdx < dataObject->getDimPaths().size(); dimIdx++)
                {
                    auto dimPath = dataObject->getDimPaths()[dimIdx];
                    std::string dimName = "";
                    if (namedPathDims.find(dimPath) != namedPathDims.end())
                    {
                        dimName = namedPathDims[dimPath];
                    }
                    else
                    {
                        auto newDimStr = std::ostringstream();

                        if (dimIdx == 0)  // First dim corresponds to variables "Location"
                        {
                            newDimStr << "nlocs_" << dataObject->getGroupByFieldName();
                        }
                        else
                        {
                            newDimStr << "dim_" << autoGenDimNumber;
                        }

                        dimName = newDimStr.str();
                        namedPathDims[dimPath] = dimName;
                        autoGenDimNumber++;
                    }

                    if (knownDims.find(dimPath) == knownDims.end())
                    {
                        knownDims[dimPath] = ioda::NewDimensionScale<int>(dimName, dataObject->getDims()[dimIdx]);
                    }
                }
            }

            ioda::NewDimensionScales_t allDims;
            for (auto dimPair : knownDims)
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
                    auto dimVar = obsGroup.vars[namedPathDims[dimPath]];
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
                var.atts.add<std::string>("units", { varDesc.units }, {1});

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

  bool IodaEncoder::isInteger(const std::string& str) const
  {
      bool isInt = true;
      if (str.empty())
      {
          isInt = false;
      }
      else
      {
          for (auto it = str.begin(); it != str.end(); it++)
          {
              if (!std::isdigit(*it))
              {
                isInt = false;
                break;
              }
          }
      }

      return isInt;
  }
}  // namespace Ingester
