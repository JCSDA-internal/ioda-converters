/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "ObsGroupDescription.h"

using namespace Ingester;

void ObsGroupDescription::addMetaKeys(FieldName& fieldName, ObsGroupName& obsGroupName)
{
    metaDataFieldMap_.insert({fieldName, obsGroupName});
}

void ObsGroupDescription::addObsValKeys(FieldName& fieldName, ObsGroupName& obsGroupName)
{
    obsValFieldMap_.insert({fieldName, obsGroupName});
}
