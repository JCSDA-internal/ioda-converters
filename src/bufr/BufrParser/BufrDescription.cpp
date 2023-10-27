/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <ostream>
#include <memory>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "BufrDescription.h"


namespace
{
    namespace ConfKeys
    {
        const char* Filename = "obsdatain";
        const char* TablePath = "tablepath";
        const char* Exports = "exports";
    }  // namespace ConfKeys
}  // namespace

namespace Ingester
{
    BufrDescription::BufrDescription(const std::string& yamlPath) :
        export_(Export())
    {
        auto conf = eckit::YAMLConfiguration(eckit::PathName(yamlPath));

        auto obsspaceConf = conf.getSubConfiguration("observations")
                                .getSubConfiguration("obs space")
                                .getSubConfiguration("obs space");

        init(obsspaceConf);
    }

    BufrDescription::BufrDescription(const eckit::Configuration &conf) :
        export_(Export())
    {
        init(conf);
    }

    void BufrDescription::init(const eckit::Configuration &conf)
    {
        export_ = Export(conf.getSubConfiguration(ConfKeys::Exports));

        setFilepath(conf.getString(ConfKeys::Filename));

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
