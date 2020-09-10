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
    typedef struct
    {
        float start;
        float end;
    } Range;

    typedef struct
    {
        std::string name;
        std::string size;
    } ScaleDescription;

    typedef struct
    {
        std::string name;
        std::string source;
        std::vector<std::string> scales;
        std::string longName;
        std::string units;
        std::shared_ptr<std::string> coordinates; //Optional
        std::shared_ptr<Range> range; //Optional
    } VariableDescription;

    typedef std::vector<ScaleDescription> ScaleDescriptions;
    typedef std::vector<VariableDescription> VariableDescriptions;

    class IodaDescription
    {
     public:
        IodaDescription() = default;
        explicit IodaDescription(const eckit::Configuration& conf, const std::string& basepath="");

        void addScale(ScaleDescription scale);
        void addVariable(VariableDescription variable);

        inline ScaleDescriptions getScales() const { return scales_; };
        inline VariableDescriptions getVariables() const { return variables_; };
        inline std::string getFilepath() const { return filepath_; }

     private:
        std::string filepath_;
        ScaleDescriptions scales_;
        VariableDescriptions variables_;


    };
}  // namespace Ingester
