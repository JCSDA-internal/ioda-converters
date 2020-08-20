//
// Created by Ronald McLaren on 8/17/20.
//

#include "FieldMap.h"

using namespace eckit;


FieldMap::FieldMap(const Configuration& conf)
{
    if (conf.has("longitude"))
    {
        conf.getSubConfigurations("longitude");
    }

    if (conf.has("latitude"))
    {
        conf.getSubConfigurations("longitude");
    }

    if (conf.has("timestamp"))
    {
        conf.getSubConfigurations("timestamp");
    }

    if (conf.has("observations"))
    {
        for (const auto &obs : conf.getSubConfigurations("observations"))
        {

        }
    }
}


