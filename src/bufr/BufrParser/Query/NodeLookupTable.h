/*
 * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>
#include <vector>
#include <unordered_map>
#include <unordered_set>

#include "VectorMath.h"
#include "DataProvider/DataProvider.h"
#include "SubsetTable.h"
#include "Target.h"
#include "Constants.h"

namespace Ingester {
namespace bufr {

    namespace __details
    {
        /// \brief BUFR messages are indexed according to start and stop values that are dependant
        /// on the message itself (the indexing is a property of the message). This object allows
        /// lets you make an array where the indexing is offset with respect to the actual position
        /// of the object in the array.
        template <typename T>
        class OffsetArray
        {
         public:
            OffsetArray(size_t startIdx, size_t endIdx)
                : offset_(startIdx)
            {
                data_.resize(endIdx - startIdx + 1);
            }

            OffsetArray(size_t startIdx, size_t endIdx, const T& initVal)
                : offset_(startIdx)
            {
                data_.resize(endIdx - startIdx + 1, initVal);
            }

            T& operator[](size_t idx) { return data_[idx - offset_]; }

         private:
            std::vector<T> data_;
            size_t offset_;
        };
    }  // namespace __details


    struct Data
    {
        union Value
        {
            std::vector<double> octets;
            std::vector<std::string> strings;

            Value() {}
            ~Value() {}

            void initOctet()
            {
                new (&octets) std::vector<double>();
            }

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

        Data(bool isLongString)
            : isLongString(isLongString)
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

        void operator=(const Data& other)
        {
            if (isLongString)
            {
                value.strings = other.value.strings;
            }
            else
            {
                value.octets = other.value.octets;
            }
        }

        void operator=(Data&& other)
        {
            if (isLongString)
            {
                value.strings = std::move(other.value.strings);
            }
            else
            {
                value.octets = std::move(other.value.octets);
            }
        }

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

        void push_back(double val)
        {
            value.octets.push_back(val);
        }

        void push_back(const std::string& val)
        {
            value.strings.push_back(val);
        }

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

        bool isLongStr() const
        {
            return isLongString;
        }

     private:
        bool isLongString;
    };


    /// \brief Lookup table that maps BUFR subset node ids to the data and counts found in the BUFR
    /// message subset data section. This makes it possible to quickly access the data and counts
    /// information for a given node.
    class NodeLookupTable
    {
     public:
        typedef std::vector<int> CountsVector;

        struct NodeMetaData
        {
            TargetComponent component;
            std::string longStrId;
            bool collectedCounts = false;
            bool collectedData = false;
        };

        struct NodeData
        {
            Data data;
            CountsVector counts;
        };

        typedef __details::OffsetArray<NodeData> LookupTable;
        typedef __details::OffsetArray<NodeMetaData> MetaDataLookup;

        NodeLookupTable(const std::shared_ptr<DataProvider>& dataProvider, const Targets& targets );

        /// \brief Returns the NodeData for a given bufr node.
        /// \param[in] nodeId The id of the node to get the data for.
        /// \return The NodeData for the given node.
        NodeData& operator[](size_t nodeId) { return lookupTable_[nodeId]; }

     private:
        const std::shared_ptr<DataProvider> dataProvider_;
        LookupTable lookupTable_;

        /// \brief Creates a lookup table that maps node ids to NodeData objects.
        /// \param[in] targets The targets to create the lookup table for.
        /// \return The lookup table.
        LookupTable makeLookupTable(const Targets& targets) const;

        /// \brief Adds the counts data for the given targets to the lookup table.
        /// \param[in] targets The targets to add the counts data for.
        /// \param[in, out] lookup The lookup table to add the counts data to.
        void addCounts(const Targets& targets,
                       LookupTable& lookup,
                       MetaDataLookup& metaLookup) const;

        /// \brief Adds the data for the given targets to the lookup table.
        /// \param[in] targets The targets to add the data for.
        /// \param[in, out] lookup The lookup table to add the data to.
        void addData(const Targets& targets, LookupTable& lookup, MetaDataLookup& metaLookup) const;
    };
}  // namespace bufr
}  // namespace Ingester
