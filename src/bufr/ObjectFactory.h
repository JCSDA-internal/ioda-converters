/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <unordered_map>
#include <memory>
#include <string>
#include <ostream>

#include "eckit/exception/Exceptions.h"


namespace Ingester
{
    /// \brief Factory that is used to create objects
    /// \tparam U The type of class to make (base class)
    /// \tparam Args Parameters used in object constructor.
    template<class U, typename... Args>
    class ObjectFactory
    {
        /// \brief Base class for all ObjectMakers. Makes it possible to store all types of objects
        /// inside a std data structure like a map.
        class ObjectMakerBase
        {
         public:
            virtual ~ObjectMakerBase() {}
            /// \brief Instantiate a object instance
            /// \param args List of arguments required to construct the object
            /// \returns shared_ptr<U> (base class) for the the constructed object defined for this
            ///          ObjectFactory.
            virtual std::shared_ptr<U> make(Args... args) = 0;
        };

        /// \brief ObjectMaker class template definition
        /// \tparam T The concrete type of class this Maker will make
        template <class T>
        class ObjectMaker : public ObjectMakerBase
        {
         public:
            /// \brief Instantiate a object instance
            /// \param args List of arguments required to construct the object
            /// \returns shared_ptr<U> (base class) for the the constructed object defined for this
            ///          ObjectFactory.

            virtual ~ObjectMaker() {}

            std::shared_ptr<U> make(Args... args) override
            {
                return std::make_shared<T>(args...);
            }
        };

     public:
        virtual ~ObjectFactory() = default;

        /// \brief Create an object
        /// \param objectName The name associated with the object we want to make
        /// \param args The arguments required by the objects constructor
        /// \returns shared_ptr<U> (base class) for the the constructed object defined for this
        ///          ObjectFactory. NOTE: If possible design your objects with generic interfaces
        ///          that are accessible through the base class to avoid using dynamic_pointer_cast.
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

        /// \brief Register a new object type we want to be able to create
        /// \param objectName The name to associate with the new object type.
        /// \tparam T The concrete object type to register.
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
        std::unordered_map<std::string, std::shared_ptr<ObjectMakerBase>> makers_;
    };
}  // namespace Ingester
