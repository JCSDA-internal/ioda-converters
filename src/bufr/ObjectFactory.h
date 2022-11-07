/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <map>
#include <memory>
#include <string>
#include <ostream>

#include "eckit/exception/Exceptions.h"


namespace Ingester
{
    /// \brief Factory that is used to create Parsers
    /// \tparam U The type of class to make (base class)
    /// \tparam Args Parameters used in object constructor.
    template<class U, typename... Args>
    class ObjectFactory
    {
        /// \brief Base class for all ParserMakers. Makes it possible to store all types of parsers
        /// inside a std data structure like a map.
        class ObjectMakerBase
        {
         public:
            /// \brief Instantiate a Parser instance
            /// \param conf Configuration to base the Parser on (obs space)
            virtual std::shared_ptr<U> make(Args... args) = 0;
        };

        /// \brief ParserMaker class template definition
        /// \tparam T The concrete type of class this Maker will make
        template <class T>
        class ObjectMaker : public ObjectMakerBase
        {
         public:
            std::shared_ptr<U> make(Args... args) override
            {
                return std::make_shared<T>(args...);
            }
        };

     public:
        virtual ~ObjectFactory() = default;

        /// \brief Create a Parser
        /// \param conf Provides the name of the parser we want to create
        std::shared_ptr<U> create(const std::string& objectName, Args... args)
        {
            if (makers_.find(objectName) == makers_.end())
            {
                std::ostringstream errStr;
                errStr << "Trying to use unregistered object named " << objectName << ".";
                throw eckit::BadParameter(errStr.str());
            }

            return makers_[objectName]->make(args...);
        }

        /// \brief Register a new Parser type we want to be able to create
        /// \param name The name to associate with the parser class.
        /// \tparam T The concrete object type to make.
        template<class T>
        void registerObject(const std::string& objectName)
        {
            if (makers_.find(objectName) != makers_.end())
            {
                std::ostringstream errStr;
                errStr << "Trying to add object with a duplicate name ";
                errStr << objectName;
                errStr << ". Name must be unique.";

                throw eckit::BadParameter(errStr.str());
            }

            makers_.insert({objectName, std::make_shared<ObjectMaker<T>>()});
        }

     private:
        std::map<std::string, std::shared_ptr<ObjectMakerBase>> makers_;
    };
}  // namespace Ingester


// auto factory = ObjectFactory<Parser>();
// factory.register<BufrParser>("bufr");


