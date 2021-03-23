/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "IodaEncoder.h"

#include <memory>
#include <type_traits>

#include "eckit/exception/Exceptions.h"
#include "ioda/Layout.h"

#include <boost/algorithm/string.hpp>

namespace
{
    const bool USE_OLD_LAYOUT = true;
}  // namespace

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

            bool foundInvalidDim = false;
            ioda::NewDimensionScales_t newDims;
            for (const auto& scale : description_.getDims())
            {
                std::size_t size = 0;
                if (isInteger(scale.size))
                {
                    size = std::stoi(scale.size);
                }
                else
                {
                    std::string token = scale.size.substr(scale.size.find('.') + 1,
                                                          scale.size.size());

                    std::string varName = scale.size.substr(0,
                                                            scale.size.find('.'));

                    if (token == "ncols")
                    {
                        size = dataContainer->get(varName, categories)->ncols();
                    }
                    else if (token == "nrows")
                    {
                        size = dataContainer->get(varName, categories)->nrows();
                    }
                    else
                    {
                        std::ostringstream errStr;
                        errStr << "Tried to get unknown parameter " <<  token;
                        errStr << " from " << varName;
                        throw eckit::BadParameter(errStr.str());
                    }
                }

                auto newDim = std::make_shared<ioda::NewDimensionScale<int>>(scale.name,
                                                                             size,
                                                                             size,
                                                                             size);
                newDims.push_back(newDim);

                if (size <= 0) { foundInvalidDim = true; }
            }

            if (foundInvalidDim)
            {
                continue;
            }

            ioda::detail::DataLayoutPolicy::Policies policy;
            if (USE_OLD_LAYOUT)
            {
                policy = ioda::detail::DataLayoutPolicy::Policies::None;
            }
            else
            {
                policy = ioda::detail::DataLayoutPolicy::Policies::ObsGroup;
            }

            auto layoutPolicy = ioda::detail::DataLayoutPolicy::generate(policy);

            auto obsGroup = ioda::ObsGroup::generate(rootGroup, newDims, layoutPolicy);

            auto scaleMap = std::map<std::string, ioda::Variable>();
            for (const auto& scale : description_.getDims())
            {
                scaleMap.insert({scale.name, obsGroup.vars[scale.name]});
            }

            // Todo: Delete with USE_OLD_LAYOUT
            std::map<std::string, std::string> varGroupMap;
            for (const auto& varDesc : description_.getVariables())
            {
                varGroupMap.insert(splitVar(varDesc.name));
            }
            // Todo: Delete with USE_OLD_LAYOUT

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
                for (size_t dimIdx = 0; dimIdx < varDesc.dimensions.size(); dimIdx++)
                {
                    auto dimVar = scaleMap.at(varDesc.dimensions[dimIdx]);
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

                auto data = dataContainer->get(varDesc.source, categories);
                auto var = data->createVariable(obsGroup,
                                                varDesc.name,
                                                dimensions,
                                                chunks,
                                                varDesc.compressionLevel);



                var.atts.add<std::string>("long_name", { varDesc.longName }, {1});
                var.atts.add<std::string>("units", { varDesc.units }, {1});

                if (varDesc.coordinates)
                {
                    auto coordStr = *varDesc.coordinates;

                    if (USE_OLD_LAYOUT)
                    {
                        coordStr = fixCoordinatesStr(coordStr, varGroupMap);
                    }

                    var.atts.add<std::string>("coordinates", { coordStr }, {1});
                }

                if (varDesc.range)
                {
                    var.atts.add<FloatType>("valid_range",
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

  // Todo: Delete with USE_OLD_LAYOUT
  std::string IodaEncoder::fixCoordinatesStr(const std::string& coordStr,
                                             std::map<std::string, std::string> varMap)
  {
      std::vector<std::string> strs;
      boost::split(strs, coordStr, boost::is_any_of(" "));

      std::stringstream newCoordStr;
      for (auto str : strs)
      {
          if (varMap.find(str) != varMap.end())
          {
              newCoordStr << str << "@" << varMap.at(str) << " ";
          }
          else
          {
              newCoordStr << str << " ";
          }
      }

      return newCoordStr.str();
  }
  // Todo: Delete with USE_OLD_LAYOUT

  // Todo: Delete with USE_OLD_LAYOUT
  std::pair<std::string, std::string> IodaEncoder::splitVar(const std::string& varNameStr)
  {
      std::vector<std::string> parts;
      boost::split(parts, varNameStr, boost::is_any_of("@"));
      return std::pair<std::string, std::string>(parts[0], parts[1]);
  }
  // Todo: Delete with USE_OLD_LAYOUT
}  // namespace Ingester
