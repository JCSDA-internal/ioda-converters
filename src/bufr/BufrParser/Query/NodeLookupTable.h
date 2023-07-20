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
#include <variant>

#include "VectorMath.h"
#include "DataProvider/DataProvider.h"
#include "SubsetTable.h"
#include "Target.h"

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

            T& operator[](size_t idx) { return data_[idx - offset_]; }

         private:
            std::vector<T> data_;
            size_t offset_;
        };
    }  // namespace __details


    /// \brief Lookup table that maps BUFR subset node ids to the data and counts found in the BUFR
    /// message subset data section. This makes it possible to quickly access the data and counts
    /// information for a given node.
    class NodeLookupTable
    {
        typedef std::variant<std::vector<std::string>, std::vector<double>> _Data;
        typedef std::vector<int> CountsVector;

        struct _DataVector
        {
            _Data data;

            void reserve(size_t size)
            {
                if (std::holds_alternative<std::vector<double>>(data))
                {
                    std::get<std::vector<double>>(data).reserve(size);
                }
                else
                {
                    std::get<std::vector<std::string>>(data).reserve(size);
                }
            }

            bool empty() const
            {
                if (std::holds_alternative<std::vector<double>>(data))
                {
                    return std::get<std::vector<double>>(data).empty();
                }
                else
                {
                    return std::get<std::vector<std::string>>(data).empty();
                }
            }

            size_t size() const
            {
                if (std::holds_alternative<std::vector<double>>(data))
                {
                    return std::get<std::vector<double>>(data).size();
                }
                else
                {
                    return std::get<std::vector<std::string>>(data).size();
                }
            }

            void push_back(const double& item)
            {
                std::get<std::vector<double>>(data).push_back(item);
            }

            void push_back(const std::string& item)
            {
                std::get<std::vector<std::string>>(data).push_back(item);
            }

            template<typename T>
            T rawData() const
            {
                return std::get<T>(data);
            }
        };

        struct NodeData
        {
            _DataVector data;
            CountsVector counts;
            TargetComponent component;
            bool collectedCounts = false;
            bool collectedData = false;
            bool isLongString = false;
            std::string longStrId;
        };

        typedef __details::OffsetArray<NodeData> LookupTable;

     public:
        typedef _DataVector DataVector;
        typedef _Data Data;

        NodeLookupTable(const std::shared_ptr<DataProvider>& dataProvider, const Targets& targets);

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
        void addCounts(const Targets& targets, LookupTable& lookup) const;

        /// \brief Adds the data for the given targets to the lookup table.
        /// \param[in] targets The targets to add the data for.
        /// \param[in, out] lookup The lookup table to add the data to.
        void addData(const Targets& targets, LookupTable& lookup) const;
    };
}  // namespace bufr
}  // namespace Ingester
