/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "QueryVariable.h"

#include <ostream>

#include "eckit/exception/Exceptions.h"

#include "IngesterTypes.h"
#include "Transforms/TransformBuilder.h"

namespace
{
    namespace ConfKeys
    {
        const char *Query = "query";
        const char *Type = "type";
    }
}


namespace Ingester
{
    QueryVariable::QueryVariable(const std::string& exportName,
                                 const std::string& groupByField,
                                 const eckit::LocalConfiguration& conf) :
        Variable(exportName, groupByField, conf)
    {
        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> QueryVariable::exportData(const BufrDataMap& map)
    {
        if (map.find(getExportName()) == map.end())
        {
            std::stringstream errStr;
            errStr << "Export named " << getExportName();
            errStr << " could not be found during export.";
            throw eckit::BadParameter(errStr.str());
        }

        auto dataObject = map.at(getExportName());

        for (const auto& transform : TransformBuilder::makeTransforms(conf_))
        {
            transform->apply(dataObject);
        }

        return dataObject;
    }

    QueryList QueryVariable::makeQueryList() const
    {
        auto queries = QueryList();

        QueryInfo info;
        info.name = getExportName();
        info.query = conf_.getString(ConfKeys::Query);
        info.groupByField = groupByField_;

        if (conf_.has(ConfKeys::Type))
        {
            info.type = conf_.getString(ConfKeys::Type);
        }

        queries.push_back(info);

        return queries;
    }
}  // namespace Ingester
