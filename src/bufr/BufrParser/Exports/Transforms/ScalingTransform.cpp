//
// Created by Ronald McLaren on 9/24/20.
//

#include "ScalingTransform.h"


namespace Ingester
{
    ScalingTransform::ScalingTransform(double scaling) :
      scaling_(scaling)
    {
    }

    void ScalingTransform::apply(IngesterArray& array)
    {
        array = array * scaling_;
    }
}  // namespace Ingester
