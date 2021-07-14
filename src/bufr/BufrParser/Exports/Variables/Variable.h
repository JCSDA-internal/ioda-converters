/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <vector>
#include <string>
#include <memory>

#include "ResultSet.h"

#include "IngesterTypes.h"
#include "DataObject/DataObject.h"

namespace Ingester
{
    typedef std::map<std::string, std::string> QueryMap;

    /// \brief Abstract base class for all Exports.
    class Variable
    {
     public:
        Variable() = delete;

        explicit Variable(const std::string& exportName) : exportName_(exportName) {}

        virtual ~Variable() = default;

        /// \brief Variable data objects for previously parsed data from BufrDataMap.
        virtual std::shared_ptr<DataObject> exportData(const BufrDataMap& dataMap) = 0;

        /// \brief Get Query Map
        inline QueryMap getQueryMap() { return queryMap_; }

        /// \brief Get Export Name
        inline std::string getExportName() const { return exportName_; }

    protected:
        inline void initQueryMap() { queryMap_ = makeQueryMap(); }

        /// \brief Make a map of name and queries
        virtual QueryMap makeQueryMap() const = 0;

    private:
        /// \brief Name used to export this variable
        std::string exportName_;

        /// \brief The query map for all the queries this vairable needs
        QueryMap queryMap_;
    };
}  // namespace Ingester


