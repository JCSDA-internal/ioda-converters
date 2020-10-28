/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "IodaDescription.h"

#include <boost/algorithm/string.hpp>

#include "eckit/exception/Exceptions.h"

namespace
{
    namespace ConfKeys
    {
        const char* Backend = "backend";
        const char* Filename = "obsdataout";
        const char* Dimensions = "dimensions";
        const char* Variables = "variables";

        namespace Dimension
        {
            const char* Name = "name";
            const char* Size = "size";
        }  // Dimension

        namespace Variable
        {
            const char* Name = "name";
            const char* Source = "source";
            const char* Dimensions = "dimensions";
            const char* LongName = "longName";
            const char* Units = "units";
            const char* Range = "range";
            const char* Coords = "coordinates";
        }  // namespace Variable
    }  // namespace ConfKeys
}  // namespace

namespace Ingester
{
    IodaDescription::IodaDescription(const eckit::Configuration& conf)
    {
        if (conf.has(ConfKeys::Backend))
        {
            setBackend(conf.getString(ConfKeys::Backend));
        }
        else
        {
            throw eckit::BadParameter("Forgot to specify the ioda::backend.");
        }

        if (conf.has(ConfKeys::Filename))
        {
            filepath_ = conf.getString(ConfKeys::Filename);
        }
        else
        {
            if (backend_ == ioda::Engines::BackendNames::ObsStore)
            {
                filepath_ = "";
            }
            else
            {
                throw eckit::BadParameter("Filename is required with the configured backend.");
            }
        }

        for (const auto& scaleConf : conf.getSubConfigurations(ConfKeys::Dimensions))
        {
            DimensionDescription scale;
            scale.name = scaleConf.getString(ConfKeys::Dimension::Name);
            scale.size = scaleConf.getString(ConfKeys::Dimension::Size);

            addDimension(scale);
        }

        for (const auto& varConf : conf.getSubConfigurations(ConfKeys::Variables))
        {
            VariableDescription variable;
            variable.name = varConf.getString(ConfKeys::Variable::Name);
            variable.source = varConf.getString(ConfKeys::Variable::Source);
            variable.dimensions = varConf.getStringVector(ConfKeys::Variable::Dimensions);
            variable.longName = varConf.getString(ConfKeys::Variable::LongName);
            variable.units = varConf.getString(ConfKeys::Variable::Units);

            if (varConf.has(ConfKeys::Variable::Coords))
            {
                variable.coordinates =
                    std::make_shared<std::string> (varConf.getString(ConfKeys::Variable::Coords));
            }
            else
            {
                variable.coordinates = nullptr;
            }

            if (varConf.has(ConfKeys::Variable::Range))
            {
                auto range = std::make_shared<Range>();
                range->start = std::stoi(varConf.getStringVector(ConfKeys::Variable::Range)[0]);
                range->end = std::stoi(varConf.getStringVector(ConfKeys::Variable::Range)[1]);
                variable.range = range;
            }
            else
            {
                variable.range = nullptr;
            }

            addVariable(variable);
        }
    }

    void IodaDescription::addDimension(const DimensionDescription& scale)
    {
        dimensions_.push_back(scale);
    }

    void IodaDescription::addVariable(const VariableDescription& variable)
    {
        variables_.push_back(variable);
    }

    void IodaDescription::setBackend(const std::string& backend)
    {
        auto backend_lowercase = boost::algorithm::to_lower_copy(backend);
        if (backend_lowercase == "netcdf")
        {
            setBackend(ioda::Engines::BackendNames::Hdf5File);
        }
        else if (backend_lowercase == "inmemory" || backend_lowercase == "in-memory")
        {
            setBackend(ioda::Engines::BackendNames::ObsStore);
        }
        else
        {
            throw eckit::BadParameter("Unknown ioda::backend specified.");
        }
    }
}  // namespace Ingester
