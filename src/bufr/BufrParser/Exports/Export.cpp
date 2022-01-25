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
            const char* Datetime = "dateTime";
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
            const char* Mnemonic = "mnemonic";
            const char* Bounding = "bounding";
            const char* UpperBound = "upperBound";
            const char* LowerBound = "lowerBound";
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
        if (conf.keys().size() == 0)
        {
            std::stringstream errStr;
            errStr << "bufr::exports::variables must contain a dictionary of variables!";
            throw eckit::BadParameter(errStr.str());
        }

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
                errMsg << "Unknown bufr::exports::variable of type " << key;
                throw eckit::BadParameter(errMsg.str());
            }

            variables_.insert({key, variable});
        }
    }

    void Export::addSplits(const eckit::Configuration &conf)
    {
        if (conf.keys().size() == 0)
        {
            std::stringstream errStr;
            errStr << "bufr::exports::splits must contain a dictionary of splits!";
            throw eckit::BadParameter(errStr.str());
        }

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
                errMsg << "Unknown bufr::exports::splits of type " << key;
                throw eckit::BadParameter(errMsg.str());
            }

            splits_.insert({key, split});
        }
    }

    void Export::addFilters(const eckit::Configuration &conf)
    {
        auto subConfs = conf.getSubConfigurations();
        if (subConfs.size() == 0)
        {
            std::stringstream errStr;
            errStr << "bufr::exports::filters must contain a list of filters!";
            throw eckit::BadParameter(errStr.str());
        }

        for (const auto& subConf : subConfs)
        {
            std::shared_ptr<Filter> filter;

            if (subConf.has(ConfKeys::Filter::Bounding))
            {
                auto filterConf = subConf.getSubConfiguration(ConfKeys::Filter::Bounding);

                std::shared_ptr<float> lowerBound = nullptr;
                if (filterConf.has(ConfKeys::Filter::LowerBound))
                {
                    lowerBound = std::make_shared<float>(
                        filterConf.getFloat(ConfKeys::Filter::LowerBound));
                }

                std::shared_ptr<float> upperBound = nullptr;
                if (filterConf.has(ConfKeys::Filter::UpperBound))
                {
                    upperBound = std::make_shared<float>(
                        filterConf.getFloat(ConfKeys::Filter::UpperBound));
                }

                filter = std::make_shared<BoundingFilter>(
                    filterConf.getString(ConfKeys::Filter::Mnemonic),
                    lowerBound,
                    upperBound);
            }
            else
            {
                std::ostringstream errMsg;
                errMsg << "bufr::exports::filters Unknown filter of type ";
                errMsg << subConf.keys()[0] << ".";
                throw eckit::BadParameter(errMsg.str());
            }

            filters_.push_back(filter);
        }
    }

}  // namespace Ingester
