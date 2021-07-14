/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <ostream>
#include <memory>

#include "eckit/exception/Exceptions.h"
#include "oops/util/IntSetParser.h"

#include "BufrDescription.h"


namespace
{
    namespace ConfKeys
    {
        const char* Filename = "obsdatain";
        const char* IsWmoFormat = "isWmoFormat";
        const char* TablePath = "tablepath";
        const char* Exports = "exports";
    }  // namespace ConfKeys
}  // namespace

namespace Ingester
{
    BufrDescription::BufrDescription(const eckit::Configuration &conf) :
        export_(Export(conf.getSubConfiguration(ConfKeys::Exports)))
    {
        setFilepath(conf.getString(ConfKeys::Filename));

        if (conf.has(ConfKeys::IsWmoFormat) && conf.getBool(ConfKeys::IsWmoFormat))
        {
            setIsWmoFormat(true);
        }
        else
        {
            setIsWmoFormat(false);
        }

        if (conf.has(ConfKeys::TablePath))
        {
            setTablepath(conf.getString(ConfKeys::TablePath));
        }
        else
        {
            setTablepath("");
        }
    }
}  // namespace Ingester
