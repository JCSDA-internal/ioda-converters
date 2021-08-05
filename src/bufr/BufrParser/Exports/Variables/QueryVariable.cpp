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
                                 const std::string& forField,
                                 const Transforms& transforms) :
        Variable(exportName),
        query_(query),
        forField_(forField),
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
            throw eckit::BadParameter(errStr.str());
        }

        auto data = map.at(getExportName());

        if (auto arrDataObj = std::dynamic_pointer_cast<ArrayDataObject>(data))
        {
            auto arr = arrDataObj->get();
            applyTransforms( arr);
            data = std::make_shared<ArrayDataObject>(arr);
        }
        else
        {
            if (transforms_.size() > 0)
            {
                std::stringstream errStr;
                errStr << "Tried to apply transform to a string field in ";
                errStr << getExportName() << ".";
                throw eckit::BadParameter(errStr.str());
            }
        }

        return data;
    }

    void QueryVariable::applyTransforms(IngesterArray& data)
    {
        for (auto transform : transforms_)
        {
            transform->apply(data);
        }
    }

    QueryList QueryVariable::makeQueryList() const
    {
        auto queries = QueryList();

        QueryInfo info;
        info.name = getExportName();
        info.query = query_;
        info.forField = forField_;
        queries.push_back(info);

        return queries;
    }
}  // namespace Ingester
