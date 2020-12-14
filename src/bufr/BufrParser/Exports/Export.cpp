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

#include "Filters/GreaterFilter.h"
#include "Filters/LesserFilter.h"
#include "Filters/RangeFilter.h"
#include "Splits/CategorySplit.h"
#include "Variables/MnemonicVariable.h"
#include "Variables/DatetimeVariable.h"
#include "Variables/Transforms/Transform.h"
#include "Variables/Transforms/TransformBuilder.h"


namespace
{
    namespace ConfKeys
    {
        const char* Filters = "filters";
        const char* Splits = "splits";
        const char* Variables = "variables";

        namespace Variable
        {
            const char* Datetime = "datetime";
            const char* Mnemonic = "mnemonic";
        }  // namespace Variable

        namespace Split
        {
            const char* Category = "category";
            const char* Mnemonic = "mnemonic";
            const char* NameMap = "map";
        }  // namespace Split

        namespace Filter
        {
            const char* Range = "range";
            const char* Greater = "greaterThan";
            const char* Lesser = "lessThan";
            const char* Extents = "extents";
            const char* Value = "value";
            const char* Mnemonic = "mnemonic";
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

        if (conf.has(ConfKeys::Variables))
        {
            addVariables(conf.getSubConfiguration(ConfKeys::Variables));
        }
        else
        {
            throw eckit::BadParameter("Missing export::variables section in configuration.");
        }
    }

    void Export::addVariables(const eckit::Configuration &conf)
    {
        for (const auto& key : conf.keys())
        {
            std::shared_ptr<Variable> variable;

            auto subConf = conf.getSubConfiguration(key);
            if (subConf.has(ConfKeys::Variable::Datetime))
            {
                auto dtconf = subConf.getSubConfiguration(ConfKeys::Variable::Datetime);
                variable = std::make_shared<DatetimeVariable>(dtconf);
            }
            else if (subConf.has(ConfKeys::Variable::Mnemonic))
            {
                Transforms transforms = TransformBuilder::makeTransforms(subConf);
                variable = std::make_shared<MnemonicVariable>(
                    subConf.getString(ConfKeys::Variable::Mnemonic), transforms);
            }
            else
            {
                std::ostringstream errMsg;
                errMsg << "Unknown export::variable of type " << key;
                throw eckit::BadParameter(errMsg.str());
            }

            variables_.insert({key, variable});
        }
    }

    void Export::addSplits(const eckit::Configuration &conf)
    {
        for (const auto& key : conf.keys())
        {
            std::shared_ptr<Split> split;

            auto subConf = conf.getSubConfiguration(key);

            if (subConf.has(ConfKeys::Split::Category))
            {
                auto catConf = subConf.getSubConfiguration(ConfKeys::Split::Category);

                auto nameMap = CategorySplit::NameMap();
                if (catConf.has(ConfKeys::Split::NameMap))
                {
                    const auto& mapConf = catConf.getSubConfiguration(ConfKeys::Split::NameMap);
                    for (const std::string& mapKey : mapConf.keys())
                    {
                        auto intKey = mapKey.substr(1, mapKey.size());
                        nameMap.insert({std::stoi(intKey), mapConf.getString(mapKey)});
                    }
                }

                split = std::make_shared<CategorySplit>(
                    catConf.getString(ConfKeys::Split::Mnemonic),
                    nameMap);
            }
            else
            {
                std::ostringstream errMsg;
                errMsg << "Unknown export::split of type " << key;
                throw eckit::BadParameter(errMsg.str());
            }

            splits_.insert({key, split});
        }
    }

    void Export::addFilters(const eckit::Configuration &conf)
    {
        for (const auto& subConf : conf.getSubConfigurations())
        {
            std::shared_ptr<Filter> filter;

            if (subConf.has(ConfKeys::Filter::Range))
            {
                auto filterConf = subConf.getSubConfiguration(ConfKeys::Filter::Range);
                filter = std::make_shared<RangeFilter>(
                    filterConf.getString(ConfKeys::Filter::Mnemonic),
                    filterConf.getFloatVector(ConfKeys::Filter::Extents));
            }
            else if (subConf.has(ConfKeys::Filter::Greater))
            {
                auto filterConf = subConf.getSubConfiguration(ConfKeys::Filter::Greater);
                filter = std::make_shared<GreaterFilter>(
                    filterConf.getString(ConfKeys::Filter::Mnemonic),
                    filterConf.getFloat(ConfKeys::Filter::Value));
            }
            else if (subConf.has(ConfKeys::Filter::Lesser))
            {
                auto filterConf = subConf.getSubConfiguration(ConfKeys::Filter::Lesser);
                filter = std::make_shared<LesserFilter>(
                    filterConf.getString(ConfKeys::Filter::Mnemonic),
                    filterConf.getFloat(ConfKeys::Filter::Value));
            }
            else
            {
                std::ostringstream errMsg;
                errMsg << "Unknown export::filter of type.";
                throw eckit::BadParameter(errMsg.str());
            }

            filters_.push_back(filter);
        }
    }

}  // namespace Ingester
