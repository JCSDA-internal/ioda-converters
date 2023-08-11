/*
 * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>

#include "Constants.h"

namespace Ingester {
namespace bufr {

    /// \brief A class to hold data from a BUFR message. Unfortunatly this data can come either as
    ///        long strings (strings larger than 8 bytes) or as octets (8 byte chunks). In order to
    ///        handle both cases in a performant way we use a union to hold the data (std::variant
    ///        is too slow). This means that we need to explicitly call the destructor and
    ///        constructor for the union members. Many of the methods exposed on the class mimic
    ///        those of std::vector.
    struct Data
    {
        /// \brief A union to hold the data. Either a vector of doubles or a vector of strings.
        union Value
        {
            std::vector<double> octets;
            std::vector<std::string> strings;

            Value() {}
            ~Value() {}

            /// \brief Explicitly call the constructor for the octet vector.
            void initOctet()
            {
                new (&octets) std::vector<double>();
            }

            /// \brief Explicitly call the constructor for the octet vector.
            void initString()
            {
                new (&strings) std::vector<std::string>();
            }
        };

        Value value;

        Data() : isLongString(false)
        {
            value.initOctet();
        }

        /// \brief Constructor
        explicit Data(bool isLongString) : isLongString(isLongString)
        {
            if (isLongString)
            {
                value.initString();
            }
            else
            {
                value.initOctet();
            }
        }

        /// \brief Copy constructor
        Data(const Data& other) : isLongString(other.isLongString)
        {
            if (isLongString)
            {
                value.initString();
                value.strings = other.value.strings;
            }
            else
            {
                value.initOctet();
                value.octets = other.value.octets;
            }
        }

        /// \brief Destructor. Be careful to clean up the union here.
        ~Data()
        {
            if (isLongString)
            {
                value.strings.~vector();
            }
            else
            {
                value.octets.~vector();
            }
        }

        /// \brief Assignemnt operator defention (copy)
        void operator=(const Data& other)
        {
            isLongStr(other.isLongString);

            if (isLongString)
            {
                value.strings = other.value.strings;
            }
            else
            {
                value.octets = other.value.octets;
            }
        }

        /// \brief Assignemnt operator defention (move)
        void operator=(Data&& other)
        {
            isLongStr(other.isLongString);

            if (isLongString)
            {
                value.strings = std::move(other.value.strings);
            }
            else
            {
                value.octets = std::move(other.value.octets);
            }
        }

        /// \brief Get the size of the data.
        size_t size() const
        {
            if (isLongString)
            {
                return value.strings.size();
            }
            else
            {
                return value.octets.size();
            }
        }

        /// \brief Resize the data.
        /// \param size The new size of the data.
        void resize(size_t size)
        {
            if (isLongString)
            {
                value.strings.resize(size, MissingStringValue);
            }
            else
            {
                value.octets.resize(size, MissingOctetValue);
            }
        }

        /// \brief Reserve space for the data.
        /// \param size The size to reserve.
        void reserve(size_t size)
        {
            if (isLongString)
            {
                value.strings.reserve(size);
            }
            else
            {
                value.octets.reserve(size);
            }
        }

        /// \brief Push an octet value onto the back of the data.
        /// \param val The value to push.
        /// \note This method is only valid if the data is an octet.
        void push_back(double val)
        {
            value.octets.push_back(val);
        }

        /// \brief Push a string value onto the back of the data.
        /// \param val The value to push.
        /// \note This method is only valid if the data is a long string.
        void push_back(const std::string& val)
        {
            value.strings.push_back(val);
        }

        /// \brief Is the data empty.
        /// \return True if the data is empty.
        bool empty() const
        {
            if (isLongString)
            {
                return value.strings.empty();
            }
            else
            {
                return value.octets.empty();
            }
        }

        /// \brief Is the value stored at the given index a missing value.
        /// \param idx The index to check.
        /// \return True if the value is missing.
        bool isMissing(size_t idx) const
        {
            if (isLongString)
            {
                return value.strings[idx] == MissingStringValue;
            }
            else
            {
                return value.octets[idx] == MissingOctetValue;
            }
        }

        /// \brief Set the isLongString attribute
        /// \param isLongString True if the data is a long string.
        void isLongStr(bool isLongString)
        {
            if (isLongString)
            {
                if (!this->isLongString)
                {
                    value.octets.~vector();
                    value.initString();
                }
            }
            else
            {
                if (this->isLongString)
                {
                    value.strings.~vector();
                    value.initOctet();
                }
            }
            this->isLongString = isLongString;
        }

        /// \brief Get the isLongString attribute
        /// \return True if the data is a long string.
        bool isLongStr() const
        {
            return isLongString;
        }

    private:
        bool isLongString;
    };
}  // namespace bufr
}  // namespace Ingester
