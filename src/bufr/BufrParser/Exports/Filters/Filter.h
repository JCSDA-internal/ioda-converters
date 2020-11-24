//
// Created by Ronald McLaren on 11/11/20.
//

#pragma once

#include "IngesterTypes.h"


namespace Ingester
{
    class Filter
    {
        virtual void apply(IngesterArray& array) = 0;
    };
}  // namespace Ingester
