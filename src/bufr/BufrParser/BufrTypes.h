/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <set>
#include <map>
#include <memory>

#include "EncoderTypes.h"

namespace BufrParser
{
    class Export;

    typedef std::set<size_t> Channels;
    typedef IodaEncoder::EncoderArrayMap BufrDataMap;
    typedef std::map<std::string, std::shared_ptr<Export>> ExportMap;
}  // namespace BufrParser
