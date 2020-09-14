//
// Created by Ronald McLaren on 9/2/20.
//

#pragma once

#include <vector>
#include <string>
#include <memory>

#include "BufrParser/BufrTypes.h"
#include "DataObject/DataObject.h"

namespace Ingester
{
    class Export
    {
     public:
        virtual ~Export() = default;

        virtual std::shared_ptr<DataObject> exportData(BufrDataMap map) = 0;
    };
}  // namespace Ingester


