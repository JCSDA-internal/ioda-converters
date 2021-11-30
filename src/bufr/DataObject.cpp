/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "DataObject.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"


namespace Ingester
{
    std::shared_ptr<DataObjectBase>
        DataObjectBase::fromResult(const std::shared_ptr<bufr::ResultBase>& resultBase,
                                   const std::string& query)
    {
        std::shared_ptr<DataObjectBase> dataObject = nullptr;

        if (auto result = std::dynamic_pointer_cast<bufr::Result<float>>(resultBase))
        {
            dataObject = std::make_shared<DataObject<float>> (result->data,
                                                              result->field_name,
                                                              result->group_by_field_name,
                                                              result->dims,
                                                              query,
                                                              result->dimPaths);
        }
        else if (auto result = std::dynamic_pointer_cast<bufr::Result<std::string>>(resultBase))
        {
            dataObject = std::make_shared<DataObject<std::string>> (result->data,
                                                                    result->field_name,
                                                                    result->group_by_field_name,
                                                                    result->dims,
                                                                    query,
                                                                    result->dimPaths);
        }
        else
        {
            throw eckit::BadParameter("Encountered unsupported Result Type.");
        }

        return dataObject;
    }
}  // namespace Ingester
