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


namespace Ingester
{
    QueryVariable::QueryVariable(const std::string& exportName,
                                 const std::string& query,
                                 const std::string& groupByField,
                                 const std::string& type,
                                 const Transforms& transforms) :
        Variable(exportName),
        query_(query),
        groupByField_(groupByField),
        type_(type),
        transforms_(transforms)
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

        for (auto transform : transforms_)
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
        info.query = query_;
        info.groupByField = groupByField_;
        info.type = type_;
        queries.push_back(info);

        return queries;
    }
}  // namespace Ingester
