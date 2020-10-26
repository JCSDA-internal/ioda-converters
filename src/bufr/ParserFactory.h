/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <map>
#include <memory>
#include <string>
#include <ostream>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "Parser.h"

namespace
{
    namespace ConfKeys
    {
        const char* ParserName = "name";
    }
}

namespace Ingester
{
    /// \brief Base class for all ParserMakers. Makes it possible to store all types of parsers
    /// inside a std data structure like a map.
    class ParserMakerBase
    {
     public:
        /// \brief Instantiate a Parser instance
        /// \param conf Configuration to base the Parser on (obs space)
        virtual std::shared_ptr<Parser> make(const eckit::Configuration &conf) = 0;
    };

    /// \brief ParserMaker class template definition
    /// \tparam T The Parser class this Maker will make
    template <class T>
    class ParserMaker : public ParserMakerBase
    {
     public:
        std::shared_ptr<Parser> make(const eckit::Configuration &conf) override
        {
            return std::make_shared<T>(conf);
        }
    };

    /// \brief Factory that is used to create Parsers
    class ParserFactory
    {
     public:
        /// \brief Create a Parser
        /// \param conf Provides the name of the parser we want to create
        static std::shared_ptr<Parser> create(const eckit::Configuration &conf)
        {
            if (!conf.has(ConfKeys::ParserName))
            {
                throw eckit::BadParameter("Parser configuration has no \"name\".");
            }
            else if (getMakers().find(conf.getString(ConfKeys::ParserName)) == getMakers().end())
            {
                std::ostringstream errStr;
                errStr << "Trying to use unregistered parser named ";
                errStr << conf.getString(ConfKeys::ParserName);
                throw eckit::BadParameter(errStr.str());
            }

            return getMakers()[conf.getString(ConfKeys::ParserName)]->make(conf);
        }

        /// \brief Register a new Parser type we want to be able to create
        /// \tparam T The Parser class
        /// \param name The name to associate with the parser class.
        template<class T>
        static void registerParser(const std::string& name)
        {
            if (getMakers().find(name) != getMakers().end())
            {
                std::ostringstream errStr;
                errStr << "Trying to add parser with a duplicate name ";
                errStr << name;
                errStr << ". Name must be unique.";

                throw eckit::BadParameter(errStr.str());
            }

            std::unique_ptr<ParserMaker<T>> parserMaker;
            getMakers().insert({name, parserMaker});
        }

     private:
        /// \brief Internal method that returns a map of Parser Makers that were registered
        /// with registerParser
        static std::map<std::string, std::unique_ptr<ParserMakerBase>>& getMakers()
        {
            static std::map<std::string, std::unique_ptr<ParserMakerBase>> makers;
            return makers;
        }
    };
}  // namespace Ingester
