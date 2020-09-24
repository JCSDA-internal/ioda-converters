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

    void OffsetTransform::apply(std::shared_ptr<ArrayDataObject> array)
    {
        array = array->get().get() + offset_;
    }

}  // namespace Ingester
