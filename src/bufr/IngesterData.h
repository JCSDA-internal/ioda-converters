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
#include <typeinfo>
#include <type_traits>
#include <boost/any.hpp>

#include "Eigen/Dense"

#include "IngesterTypes.h"


namespace Ingester
{
    class IngesterData
    {
     public:
        IngesterData() = default;

        template<class T = IngesterArray>
        void add(const std::string& fieldName, const T& data)
        {
            static_assert(std::is_same<T, IngesterArray>::value || \
                          std::is_same<T, IngesterStrVector>::value,
                          "Attempted to add unsupported type to IngesterData.");

            if (hasKey(fieldName))
            {
                std::cout << "ERROR: Field called " << fieldName << " already exists." << std::endl;
                abort();
            }

            dataMap_.insert({fieldName, data});
            typeMap_.insert({fieldName, typeid(T).name()});
        }

        template<class T = IngesterArray>
        T get(const std::string& fieldName) const
        {
            if (!hasKey(fieldName))
            {
                std::cout << "ERROR: Field called " << fieldName << " doesn't exist." << std::endl;
                abort();
            }

            T result;
            try
            {
                result = boost::any_cast<T>(dataMap_.at(fieldName));
            }
            catch (std::exception& e)
            {
                std::cout << "ERROR: Field called " << fieldName << " does not have the right type." << std::endl;
                abort();
            }

            return result;
        }

        bool hasKey(const std::string& fieldName) const;

        inline unsigned int size() const { return size_; }
        inline void setSize(const unsigned int size) { size_ = size; }
        inline std::string getTypeName(std::string fieldName) { return typeMap_.at(fieldName); }

     private:
        std::map<std::string, boost::any> dataMap_;
        std::map<std::string, std::string> typeMap_;
        unsigned int size_ = 0;
    };
}  // namespace Ingester
