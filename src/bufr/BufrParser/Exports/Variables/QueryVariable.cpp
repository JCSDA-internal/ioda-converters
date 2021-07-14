/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
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
                                 const Transforms& transforms) :
        Variable(exportName),
        query_(query),
        transforms_(transforms)
    {
        initQueryMap();
    }

    std::shared_ptr<DataObject> QueryVariable::exportData(const BufrDataMap& map)
    {
        if (map.find(getExportName()) == map.end())
        {
            std::stringstream errStr;
            errStr << "Export named " << getExportName();
            errStr << " could not be found during export.";

            eckit::BadParameter(errStr.str());
        }

        auto data = map.at(getExportName());
        applyTransforms(data);
        return std::make_shared<ArrayDataObject>(data);
    }

    void QueryVariable::applyTransforms(IngesterArray& data)
    {
        for (auto transform : transforms_)
        {
            transform->apply(data);
        }
    }

    std::map<std::string, std::string> QueryVariable::makeQueryMap() const
    {
        auto queries = QueryMap();
        queries[getExportName()] = query_;
        return queries;
    }
}  // namespace Ingester
