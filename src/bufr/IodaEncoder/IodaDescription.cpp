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
        const char* Globals = "globals";

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
            const char* Chunks = "chunks";
            const char* CompressionLevel = "compressionLevel";
        }  // namespace Variable

        namespace Global
        {
            const char* Name = "name";
            const char* Value = "value";
            const char* Type = "type";
            const char* StringType = "string";
            const char* FloatType = "float";
            const char* FloatVectorType = "floatVector";
            const char* IntType = "int";
            const char* IntVectorType = "intVector";
        }  // namespace Global
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

        auto dimConfs = conf.getSubConfigurations(ConfKeys::Dimensions);
        if (dimConfs.size() == 0)
        {
            std::stringstream errStr;
            errStr << "ioda::dimensions must contain a list of dimensions!";
            throw eckit::BadParameter(errStr.str());
        }

        for (const auto& dimConf : dimConfs)
        {
            DimensionDescription scale;
            scale.name = dimConf.getString(ConfKeys::Dimension::Name);
            scale.size = dimConf.getString(ConfKeys::Dimension::Size);

            addDimension(scale);
        }

        auto varConfs = conf.getSubConfigurations(ConfKeys::Variables);
        if (varConfs.size() == 0)
        {
            std::stringstream errStr;
            errStr << "ioda::variables must contain a list of variables!";
            throw eckit::BadParameter(errStr.str());
        }

        for (const auto& varConf : varConfs)
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

            variable.range = nullptr;
            if (varConf.has(ConfKeys::Variable::Range))
            {
                auto range = std::make_shared<Range>();
                range->start = std::stoi(varConf.getStringVector(ConfKeys::Variable::Range)[0]);
                range->end = std::stoi(varConf.getStringVector(ConfKeys::Variable::Range)[1]);
                variable.range = range;
            }

            variable.chunks = {};
            if (varConf.has(ConfKeys::Variable::Chunks))
            {
                auto chunks = std::vector<ioda::Dimensions_t>();

                for (const auto& chunkStr : varConf.getStringVector(ConfKeys::Variable::Chunks))
                {
                    chunks.push_back(std::stoi(chunkStr));
                }

                variable.chunks = chunks;
            }

            variable.compressionLevel = 6;
            if (varConf.has(ConfKeys::Variable::CompressionLevel))
            {
                int compressionLevel = varConf.getInt(ConfKeys::Variable::CompressionLevel);
                if (compressionLevel < 0 || compressionLevel > 9)
                {
                    throw eckit::BadParameter("GZip compression level must be a number 0-9");
                }

                variable.compressionLevel = varConf.getInt(ConfKeys::Variable::CompressionLevel);
            }

            addVariable(variable);
        }

        if (conf.has(ConfKeys::Globals))
           {
           auto globalConfs = conf.getSubConfigurations(ConfKeys::Globals);
           for (const auto& globalConf : globalConfs)
           {
              if (globalConf.getString(ConfKeys::Global::Type) == \
                  ConfKeys::Global::StringType)
              {
                  auto global = std::make_shared<GlobalDescription<std::string>>();
                  global->name = globalConf.getString(ConfKeys::Global::Name);
                  global->value = globalConf.getString(ConfKeys::Global::Value);
                  addGlobal(global);
              }
              else if (globalConf.getString(ConfKeys::Global::Type) == \
                       ConfKeys::Global::FloatType)
              {
                  auto global = std::make_shared<GlobalDescription<float>>();
                  global->name = globalConf.getString(ConfKeys::Global::Name);
                  global->name = globalConf.getFloat(ConfKeys::Global::Name);
                  addGlobal(global);
              }
              else if (globalConf.getString(ConfKeys::Global::Type) == \
                       ConfKeys::Global::FloatVectorType)
              {
                  auto global = std::make_shared<GlobalDescription<std::vector<float>>>();
                  global->name = globalConf.getString(ConfKeys::Global::Name);
                  global->value = globalConf.getFloatVector(ConfKeys::Global::Value);
                  addGlobal(global);
              }
              else if (globalConf.getString(ConfKeys::Global::Type) == \
                       ConfKeys::Global::IntType)
              {
                  auto global = std::make_shared<GlobalDescription<int>>();
                  global->name = globalConf.getString(ConfKeys::Global::Name);
                  global->value = globalConf.getInt(ConfKeys::Global::Value);
                  addGlobal(global);
              }
              else if (globalConf.getString(ConfKeys::Global::Type) == \
                       ConfKeys::Global::IntVectorType)
              {
                  auto global = std::make_shared<GlobalDescription<std::vector<int>>>();
                  global->name = globalConf.getString(ConfKeys::Global::Name);
                  global->value = globalConf.getIntVector(ConfKeys::Global::Value);
                  addGlobal(global);
              }
              else
              {
                 throw eckit::BadParameter("Unsupported global attribute type");
              }
            }
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

    void IodaDescription::addGlobal(const std::shared_ptr<GlobalDescriptionBase>& global)
    {
        globals_.push_back(global);
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
