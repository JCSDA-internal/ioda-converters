/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "IodaDescription.h"

namespace
{
    namespace ConfKeys
    {
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
        }  // Variable
    }  // ConfKeys
}

namespace Ingester
{
    IodaDescription::IodaDescription(const eckit::Configuration& conf)
    {
        if (conf.has(ConfKeys::Filename))
        {
            filepath_ = conf.getString(ConfKeys::Filename);
        }
        else
        {
            filepath_ = "";
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

    void IodaDescription::addDimension(DimensionDescription scale)
    {
        dimensions_.push_back(scale);
    }

    void IodaDescription::addVariable(VariableDescription variable)
    {
        variables_.push_back(variable);
    }
}  // namespace Ingester
