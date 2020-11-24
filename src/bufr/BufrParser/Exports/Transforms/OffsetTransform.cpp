/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "OffsetTransform.h"


namespace BufrParser
{
    OffsetTransform::OffsetTransform(const double offset) :
      offset_(offset)
    {
    }

    void OffsetTransform::apply(IodaEncoder::EncoderArray& array)
    {
        array = array + offset_;
    }

}  // namespace BufrParser
