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
#include "ioda/Engines/Factory.h"

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
        std::vector<std::string> dimensions;
        std::string longName;
        std::string units;
        std::shared_ptr<std::string> coordinates;  // Optional
        std::shared_ptr<Range> range;  // Optional
    } VariableDescription;

    typedef std::vector<ScaleDescription> ScaleDescriptions;
    typedef std::vector<VariableDescription> VariableDescriptions;

    class IodaDescription
    {
     public:
        IodaDescription() = default;
        explicit IodaDescription(const eckit::Configuration& conf);

        void addScale(ScaleDescription scale);
        void addVariable(VariableDescription variable);

        inline void setBackend(ioda::Engines::BackendNames backend) { backend_ = backend; }
        inline void setFilepath(std::string filepath) { filepath_ = filepath; }

        inline ioda::Engines::BackendNames getBackend() const { return backend_; }
        inline std::string getFilepath() const { return filepath_; }
        inline ScaleDescriptions getScales() const { return dimensions_; }
        inline VariableDescriptions getVariables() const { return variables_; }

     private:
        ioda::Engines::BackendNames backend_;
        std::string filepath_;
        ScaleDescriptions dimensions_;
        VariableDescriptions variables_;

        void setBackend(std::string backend);
    };
}  // namespace Ingester
