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

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "Parser.h"

static const char* PARSER_NAME = "name";


namespace Ingester
{
    class ParserMakerBase
    {
     public:
        virtual std::shared_ptr<Parser> make(const eckit::Configuration &conf) = 0;
    };

    template <class T>
    class ParserMaker : public ParserMakerBase
    {
     public:
        std::shared_ptr<Parser> make(const eckit::Configuration &conf) override
        {
            return std::make_shared<T>(conf);
        }
    };

    class ParserFactory
    {
     public:
        static std::shared_ptr<Parser> create(const eckit::Configuration &conf)
        {
            if (!conf.has(PARSER_NAME))
            {
                throw eckit::BadParameter("Parser configuration has no Name.");
            }
            else if (getMakers().find(conf.getString(PARSER_NAME)) == getMakers().end())
            {
                throw eckit::BadParameter("Trying to use unregistered parser.");
            }

            return getMakers()[conf.getString(PARSER_NAME)]->make(conf);
        }

        template<class T>
        static void registerParser(std::string name)
        {
            getMakers().insert({name, std::make_unique<ParserMaker<T>>()});
        }

      private:
        static std::map<std::string, std::unique_ptr<ParserMakerBase>>& getMakers()
        {
            static std::map<std::string, std::unique_ptr<ParserMakerBase>> makers;
            return makers;
        }
    };
}  // namespace Ingester
