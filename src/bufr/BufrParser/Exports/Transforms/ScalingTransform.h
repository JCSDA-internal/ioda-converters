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
    /// \brief Multiply a floating point scaling factor to to the data.
    class ScalingTransform : public Transform
    {
     public:
        explicit ScalingTransform(const double scaling_);
        ~ScalingTransform() = default;

        /// \brief Apply transform to the given data.
        void apply(IngesterArray& array) override;

     private:
        const double scaling_;
    };
}  // namespace Ingester
