/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "Transform.h"


namespace Ingester
{
    class OffsetTransform : public Transform
    {
     public:
        explicit OffsetTransform(double offset);
        ~OffsetTransform() = default;

        void apply(IngesterArray& array) override;

     private:
        const double offset_;
    };
}  // namespace Ingester
