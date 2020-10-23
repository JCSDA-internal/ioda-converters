//
// Created by Ronald McLaren on 9/24/20.
//

#include "OffsetTransform.h"


namespace Ingester
{
    OffsetTransform::OffsetTransform(double offset) :
      offset_(offset)
    {
    }

    void OffsetTransform::apply(IngesterArray& array)
    {
        array = array + offset_;
    }

}  // namespace Ingester
