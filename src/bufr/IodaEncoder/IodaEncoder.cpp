/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "IodaEncoder.h"

#include <memory>

#include "IngesterData.h"


namespace Ingester
{
    IodaEncoder::IodaEncoder(const IodaDescription& description) :
        description_(description),
        backendType_(description.getFilepath().empty() ? ioda::Engines::BackendNames::ObsStore : \
                                                         ioda::Engines::BackendNames::Hdf5File)
    {
    }

    ioda::ObsGroup IodaEncoder::encode(const std::shared_ptr<IngesterData>& data, bool append)
    {
        auto backendParams = ioda::Engines::BackendCreationParameters();
        backendParams.fileName = description_.getFilepath();
        backendParams.openMode = ioda::Engines::BackendOpenModes::Read_Write;
        backendParams.createMode = ioda::Engines::BackendCreateModes::Truncate_If_Exists;
        backendParams.action = append ? ioda::Engines::BackendFileActions::Open : \
                                        ioda::Engines::BackendFileActions::Create;
        backendParams.flush = true;
        backendParams.allocBytes = data->size() * 8;

        auto rootGroup = ioda::Engines::constructBackend(backendType_, backendParams);

        // Create Scales
        ioda::NewDimensionScales_t newDims;
        for (const auto& scale : description_.getScales())
        {
            std::size_t size = 0;
            if (scale.size == "{LENGTH}")
            {
                size = data->size();
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
        for (const auto& scale : description_.getScales())
        {
            scaleMap.insert({scale.name, obsGroup.vars[scale.name]});
        }

        // Create Variables
        ioda::VariableCreationParameters float_params;
        float_params.chunk = true;
        float_params.compressWithGZIP();
        float_params.setFillValue<float>(-999);

        for (const auto& varDesc : description_.getVariables())
        {
            auto scales = std::vector<ioda::Variable>();
            for (const auto& scaleStr : varDesc.scales)
            {
                scales.push_back(scaleMap.at(scaleStr));
            }

            ioda::Variable var = obsGroup.vars.createWithScales<float>(varDesc.name,
                                                                       scales,
                                                                       float_params);

            if (varDesc.coordinates)
            {
                var.atts.add<std::string>("coordinates", { varDesc.coordinates }, {1});
            }

            var.atts.add<std::string>("long_name", { varDesc.longName }, {1});
            var.atts.add<std::string>("units", { varDesc.units }, {1});
            var.atts.add<float>("valid_range", { varDesc.range.start, varDesc.range.end }, {2});

            var.writeWithEigenRegular(data->get(varDesc.source));
        }

        return obsGroup;
    }
}
