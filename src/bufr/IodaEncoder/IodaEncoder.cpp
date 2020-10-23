/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "IodaEncoder.h"

#include <memory>

#include "DataContainer.h"


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

    ioda::ObsGroup IodaEncoder::encode(const std::shared_ptr<DataContainer>& dataContainer,
                                       bool append)
    {
        auto backendParams = ioda::Engines::BackendCreationParameters();

        if (description_.getBackend() == ioda::Engines::BackendNames::Hdf5File)
        {
            backendParams.fileName = description_.getFilepath();
        }

        backendParams.openMode = ioda::Engines::BackendOpenModes::Read_Write;
        backendParams.createMode = ioda::Engines::BackendCreateModes::Truncate_If_Exists;
        backendParams.action = append ? ioda::Engines::BackendFileActions::Open : \
                                        ioda::Engines::BackendFileActions::Create;
        backendParams.flush = true;
        backendParams.allocBytes = dataContainer->size();

        auto rootGroup = ioda::Engines::constructBackend(description_.getBackend(), backendParams);

        // Create Scales
        ioda::NewDimensionScales_t newDims;
        for (const auto& scale : description_.getDims())
        {
            std::size_t size = 0;
            if (scale.size == "{LENGTH}")
            {
                size = dataContainer->size();
            }
            else
            {
                size = std::stoi(scale.size);
            }

            auto newDim = std::make_shared<ioda::NewDimensionScale<int>>(scale.name,
                                                                         size,
                                                                         ioda::Unlimited,
                                                                         size);
            newDims.push_back(newDim);
        }

        auto obsGroup = ioda::ObsGroup::generate(rootGroup, newDims);

        auto scaleMap = std::map<std::string, ioda::Variable>();
        for (const auto& scale : description_.getDims())
        {
            scaleMap.insert({scale.name, obsGroup.vars[scale.name]});
        }

        // Create Variables
        for (const auto& varDesc : description_.getVariables())
        {
            auto dimensions = std::vector<ioda::Variable>();
            for (const auto& scaleStr : varDesc.dimensions)
            {
                dimensions.push_back(scaleMap.at(scaleStr));
            }

            auto data = dataContainer->get(varDesc.source);
            auto var = data->createVariable(obsGroup, varDesc.name, dimensions);

            var.atts.add<std::string>("long_name", { varDesc.longName }, {1});
            var.atts.add<std::string>("units", { varDesc.units }, {1});

            if (varDesc.coordinates)
            {
                var.atts.add<std::string>("coordinates", { varDesc.coordinates }, {1});
            }

            if (varDesc.range)
            {
                var.atts.add<float>("valid_range",
                                    {varDesc.range->start, varDesc.range->end},
                                    {2});
            }
        }

        return obsGroup;
    }
}  // namespace Ingester
