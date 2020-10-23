//
// Created by Ronald McLaren on 9/24/20.
//

#pragma once

#include "Transform.h"


namespace Ingester
{
    class ScalingTransform : public Transform
    {
     public:
        explicit ScalingTransform(double scaling_);
        ~ScalingTransform() = default;

        void apply(IngesterArray& array) override;

     private:
        const double scaling_;
    };
}  // namespace Ingester
