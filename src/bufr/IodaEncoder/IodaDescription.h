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
#include <type_traits>

#include "eckit/config/LocalConfiguration.h"
#include "ioda/Engines/Factory.h"

namespace Ingester
{
    struct Range
    {
        float start;
        float end;
    };

    struct DimensionDescription
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
        std::vector<ioda::Dimensions_t> chunks;  // Optional
        int compressionLevel;  // Optional
    };

    struct GlobalDescriptionBase
    {
        std::string name;
        std::string type;
    };

    template<typename T>
    struct GlobalDescription : GlobalDescriptionBase
    {
//        std::string name;
//        std::string value;
//        std::string type;
        T value;
    };

    typedef std::vector<DimensionDescription> DimDescriptions;
    typedef std::vector<VariableDescription> VariableDescriptions;
//    typedef std::vector<GlobalDescription> GlobalDescriptions;

    /// \brief Describes how to write data to IODA.
    class IodaDescription
    {
     public:
        IodaDescription() = default;
        explicit IodaDescription(const eckit::Configuration& conf);

        /// \brief Add Dimension defenition
        void addDimension(const DimensionDescription& scale);

        /// \brief Add Variable defenition
        void addVariable(const VariableDescription& variable);

        /// \brief Add Globals defenition
//        void addGlobal(const GlobalDescription& global);
//        std::vector<std::shared_ptr<GlobalDescriptionBase>> globals_;
        std::vector<std::shared_ptr<GlobalDescription<std::string>>> globals_;

        // Setters
        inline void setBackend(const ioda::Engines::BackendNames& backend) { backend_ = backend; }
        inline void setFilepath(const std::string& filepath) { filepath_ = filepath; }

        // Getters
        inline ioda::Engines::BackendNames getBackend() const { return backend_; }
        inline std::string getFilepath() const { return filepath_; }
        inline DimDescriptions getDims() const { return dimensions_; }
        inline VariableDescriptions getVariables() const { return variables_; }
//        inline GlobalDescriptions getGlobals() const { return globals_; }
//        inline GlobalDescription getGlobals() const { return globals_; }
//        inline GlobalDescriptionBase getGlobals() const { return globals_; }

     private:
        /// \brief The backend type to use
        ioda::Engines::BackendNames backend_;

        /// \brief The relative path of the output file to create
        std::string filepath_;

        /// \brief Collection of defined dimensions
        DimDescriptions dimensions_;

        /// \brief Collection of defined variables
        VariableDescriptions variables_;

        /// \brief Collection of defined globals
//        GlobalDescriptions globals_;
//        std::vector<std::shared_ptr<GlobalDescriptionBase>> globals_;

        /// \brief Collection of defined variables
        void setBackend(const std::string& backend);
    };
}  // namespace Ingester
