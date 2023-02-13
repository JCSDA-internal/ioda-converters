/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <unordered_map>

#include "DataProvider/DataProvider.h"
#include "Tokenizer.h"
#include "QueryParser.h"

namespace Ingester {
namespace bufr {
    struct TargetComponent
    {
        enum class Type
        {
            Subset,
            Binary,
            Repeat,
            Value,
            Unkown
        };

        std::shared_ptr<QueryComponent> queryComponent;
        size_t branch;
        Type type;

//        TargetComponent(std::shared_ptr<QueryComponent> queryComponent) :
//            queryComponent(queryComponent),
//            branch(0),
//            type(Type::Unkown)
//        {
//        }

        bool addsDimension() const
        {
            return type != Type::Binary;
        }

        void setType(const Typ& bufrTyp)
        {
            static std::unordered_map<Typ, Type> typMap = {
                {Typ::Subset, Type::Subset},
                {Typ::DelayedRep, Type::Repeat},
                {Typ::FixedRep, Type::Repeat},
                {Typ::DelayedRepStacked, Type::Repeat},
                {Typ::DelayedBinary, Type::Binary},
                {Typ::Repeat, Type::Repeat},
                {Typ::StackedRepeat, Type::Repeat},
                {Typ::Sequence, Type::Repeat},
                {Typ::Number, Type::Value},
                {Typ::Character, Type::Value}
            };

            this->type = typMap.at(bufrTyp);
        }
    };

    typedef std::vector<TargetComponent> TargetComponents;

    /// \brief The information or Meta data for a BUFR field whose data we wish to capture when
    /// we execute a query.
    struct Target
    {
        std::string name;
        std::string queryStr;
        std::string unit;
        TypeInfo typeInfo;
        size_t nodeIdx;
        bool anySubset = false;
        TargetComponents path;
        size_t numDimensions = 0;

        std::vector<std::string> dimPaths;
        std::vector<int> exportDimIdxs;
        std::vector<int> seqPath;

        Target() = default;

        void setPath(const TargetComponents& components)
        {
            seqPath.reserve(components.size());
            std::string currentPath;
            for (const auto& component : components)
            {
                if (!currentPath.empty()) currentPath.append("/");
                currentPath.append(component.queryComponent->name);
                dimPaths.push_back(currentPath);

                if (component.addsDimension() || component.queryComponent->filter.size() > 1)
                {
                    numDimensions++;
                }

                if (component.type == TargetComponent::Type::Repeat || component.type == TargetComponent::Type::Binary)
                {
                    seqPath.push_back(component.branch);
                }
            }

            path = components;
        }
    };

    typedef std::vector<std::shared_ptr<Target>> Targets;
}  // namespace bufr
}  // namespace Ingester
