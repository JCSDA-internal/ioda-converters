//
// Created by Ronald McLaren on 9/24/20.
//

#pragma once

#include "Transform.h"


namespace Ingester
{
    class OffsetTransform : public Transform
    {
     public:
        explicit OffsetTransform(double offset);
        ~OffsetTransform() = default;

        void apply(std::shared_ptr<ArrayDataObject> array) override;

     private:
        const double offset_;
    };
}  // namespace Ingester
