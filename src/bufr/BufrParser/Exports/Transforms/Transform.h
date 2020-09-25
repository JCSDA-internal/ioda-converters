//
// Created by Ronald McLaren on 9/24/20.
//

#pragma once

#include <memory>
#include <vector>

#include "IngesterTypes.h"

namespace Ingester
{
    class Transform
    {
    public:
        ~Transform() = default;
        virtual void apply(IngesterArray& array) = 0;
    };

    typedef std::vector <std::shared_ptr<Transform>> Transforms;
}  // namespace Ingester
