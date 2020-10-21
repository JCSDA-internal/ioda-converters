/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <map>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"


namespace Ingester
{
    struct Range
    {
        float start;
        float end;
    };

    struct ScaleDescription
    {
        std::string name;
        std::string size;
    };

    struct VariableDescription
    {
        std::string name;
        std::string source;
        std::vector<std::string> dimensions;
        std::string longName;
        std::string units;
        std::shared_ptr<std::string> coordinates;  // Optional
        std::shared_ptr<Range> range;  // Optional
    };

    typedef std::vector<ScaleDescription> ScaleDescriptions;
    typedef std::vector<VariableDescription> VariableDescriptions;

    class IodaDescription
    {
     public:
        IodaDescription() = default;
        explicit IodaDescription(const eckit::Configuration& conf);

        void addScale(ScaleDescription scale);
        void addVariable(VariableDescription variable);

        inline ScaleDescriptions getScales() const { return dimensions_; }
        inline VariableDescriptions getVariables() const { return variables_; }
        inline std::string getFilepath() const { return filepath_; }

     private:
        std::string filepath_;
        ScaleDescriptions dimensions_;
        VariableDescriptions variables_;
    };
}  // namespace Ingester
