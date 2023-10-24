/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "Export.h"

#include <ostream>
#include <iostream>

#include "eckit/exception/Exceptions.h"

#include "Filters/BoundingFilter.h"
#include "Splits/CategorySplit.h"
#include "Variables/QueryVariable.h"
#include "Variables/DatetimeVariable.h"
#include "Variables/WigosidVariable.h"
#include "Variables/SpectralRadianceVariable.h"
#include "Variables/RemappedBrightnessTemperatureVariable.h"
#include "Variables/AircraftAltitudeVariable.h"
#include "Variables/TimeoffsetVariable.h"
#include "Variables/SensorScanAngleVariable.h"
#include "Variables/SensorScanPositionVariable.h"
#include "ObjectFactory.h"


namespace
{
    namespace ConfKeys
    {
        const char* Filters = "filters";
        const char* Splits = "splits";
        const char* Variables = "variables";
        const char* GroupByVariable = "group_by_variable";
        const char* Subsets = "subsets";

        namespace Variable
        {
            const char* Datetime = "datetime";
            const char* Wigos = "wigos";
            const char* SpectralRadiance = "spectralRadiance";
            const char* RemappedBrightnessTemperature = "remappedBrightnessTemperature";
            const char* Timeoffset = "timeoffset";
            const char* AircraftAltitude = "aircraftAltitude";
            const char* SensorScanAngle = "sensorScanAngle";
            const char* SensorScanPosition = "sensorScanPosition";
            const char* Query = "query";
        }  // namespace Variable

        namespace Split
        {
            const char* Category = "category";
        }  // namespace Split

        namespace Filter
        {
            const char* Bounding = "bounding";
        }
    }  // namespace ConfKeys
}  // namespace

namespace Ingester
{
    Export::Export(const eckit::Configuration &conf)
    {
        if (conf.has(ConfKeys::Filters))  // Optional
        {
            addFilters(conf.getSubConfiguration(ConfKeys::Filters));
        }

        if (conf.has(ConfKeys::Splits))  // Optional
        {
            addSplits(conf.getSubConfiguration(ConfKeys::Splits));
        }

        std::string groupByVariable;
        if (conf.has(ConfKeys::GroupByVariable))  // Optional
        {
            groupByVariable = conf.getString(ConfKeys::GroupByVariable);
        }

        if (conf.has(ConfKeys::Subsets))
        {
            subsets_ = conf.getStringVector(ConfKeys::Subsets);
        }

        if (conf.has(ConfKeys::Variables))
        {
            addVariables(conf.getSubConfiguration(ConfKeys::Variables),
                         groupByVariable);
        }
        else
        {
            throw eckit::BadParameter("Missing export::variables section in configuration.");
        }

        //  Make sure the groupByVariable field is valid.
        if (conf.has(ConfKeys::GroupByVariable))
        {
            auto groupByFound = false;
            for (const auto &var : variables_)
            {
                if (var->getExportName() == groupByVariable)
                {
                    groupByFound = true;
                    break;
                }
            }

            if (!groupByFound)
            {
                throw eckit::BadParameter(
                    "Group by variable not found in export::variables section.");
            }
        }
    }

    void Export::addVariables(const eckit::Configuration &conf, const std::string& groupByField)
    {
        typedef ObjectFactory<Variable,
                              const std::string& /*exportName*/,
                              const std::string& /*groupByField*/,
                              const eckit::LocalConfiguration& /*configuration*/> VariableFactory;

        VariableFactory variableFactory;
        variableFactory.registerObject<QueryVariable>(ConfKeys::Variable::Query);
        variableFactory.registerObject<DatetimeVariable>(ConfKeys::Variable::Datetime);
        variableFactory.registerObject<WigosidVariable>(ConfKeys::Variable::Wigos);
        variableFactory.registerObject<TimeoffsetVariable>(ConfKeys::Variable::Timeoffset);
        variableFactory.registerObject<AircraftAltitudeVariable>
            (ConfKeys::Variable::AircraftAltitude);
        variableFactory.registerObject<SpectralRadianceVariable>
            (ConfKeys::Variable::SpectralRadiance);
        variableFactory.registerObject<RemappedBrightnessTemperatureVariable>
            (ConfKeys::Variable::RemappedBrightnessTemperature);
        variableFactory.registerObject<SensorScanAngleVariable>
            (ConfKeys::Variable::SensorScanAngle);
        variableFactory.registerObject<SensorScanPositionVariable>
            (ConfKeys::Variable::SensorScanPosition);

        if (conf.keys().size() == 0)
        {
            std::stringstream errStr;
            errStr << "bufr::exports::variables must contain a dictionary of variables!";
            throw eckit::BadParameter(errStr.str());
        }

        for (const auto& key : conf.keys())
        {
            auto subConf = conf.getSubConfiguration(key);

            std::shared_ptr<Variable> variable;
            if (subConf.has(ConfKeys::Variable::Query))
            {
                variable =
                    variableFactory.create(ConfKeys::Variable::Query, key, groupByField, subConf);
            }
            else
            {
                variable =
                    variableFactory.create(subConf.keys()[0],
                                            key,
                                            groupByField,
                                            subConf.getSubConfiguration(subConf.keys()[0]));
            }

            variables_.push_back(variable);
        }
    }

    void Export::addSplits(const eckit::Configuration &conf)
    {
        typedef ObjectFactory<Split,
                              const std::string& /*name*/,
                              const eckit::LocalConfiguration& /*configuration*/> SplitFactory;

        SplitFactory splitFactory;
        splitFactory.registerObject<CategorySplit>(ConfKeys::Split::Category);

        if (conf.keys().size() == 0)
        {
            std::stringstream errStr;
            errStr << "bufr::exports::splits must contain a dictionary of splits!";
            throw eckit::BadParameter(errStr.str());
        }

        for (const auto& key : conf.keys())
        {
            auto subConf = conf.getSubConfiguration(key);
            auto split = splitFactory.create(subConf.keys()[0],
                                             key,
                                             subConf.getSubConfiguration(subConf.keys()[0]));

            splits_.push_back(split);
        }
    }

    void Export::addFilters(const eckit::Configuration &conf)
    {
        typedef ObjectFactory<Filter,
                              const eckit::LocalConfiguration& /*configuration*/> FilterFactory;

        FilterFactory filterFactory;
        filterFactory.registerObject<BoundingFilter>(ConfKeys::Filter::Bounding);

        auto subConfs = conf.getSubConfigurations();
        if (subConfs.size() == 0)
        {
            std::stringstream errStr;
            errStr << "bufr::exports::filters must contain a list of filters!";
            throw eckit::BadParameter(errStr.str());
        }

        for (const auto& subConf : subConfs)
        {
            auto filter = filterFactory.create(subConf.keys()[0],
                                               subConf.getSubConfiguration(subConf.keys()[0]));
            filters_.push_back(filter);
        }
    }
}  // namespace Ingester
