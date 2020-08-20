/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <map>
#include <string>


namespace Ingester
{
    typedef std::string FieldName;
    typedef std::string ObsGroupName;
    typedef std::map<std::string, std::string> NameMap;

    class ObsGroupDescription
    {
    public:
        ObsGroupDescription() = default;

        void addMetaKeys(FieldName& fieldName, ObsGroupName& obsGroupName);
        void addObsValKeys(FieldName& fieldName, ObsGroupName& obsGroupName);

    private:
        NameMap metaDataFieldMap_;
        NameMap obsValFieldMap_;
    };
}
