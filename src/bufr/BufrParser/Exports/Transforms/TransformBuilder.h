//
// Created by Ronald McLaren on 9/25/20.
//

#pragma once

#include <memory.h>

#include "eckit/config/LocalConfiguration.h"

#include "Transform.h"


namespace Ingester
{
    class TransformBuilder
    {
     public:
        static std::shared_ptr<Transform> makeTransform(const eckit::Configuration& conf);
        static Transforms makeTransforms(const eckit::Configuration& conf);
    };
}  // namespace Ingester
