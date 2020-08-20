/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BufrCollector.h"

using namespace Ingester;

BufrCollector::BufrCollector(const int fileUnit, const BufrAccumulator accumulator) :
    fileUnit_(fileUnit),
    accumulator_(accumulator)
{
}

