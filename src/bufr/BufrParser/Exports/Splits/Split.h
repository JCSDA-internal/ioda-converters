//
// Created by Ronald McLaren on 11/11/20.
//

#pragma once

#include <string>
#include <vector>

#include "BufrParser/BufrTypes.h"

namespace Ingester
{
    class Split
    {
     public:
        Split() = default;

        virtual std::vector<std::string> subCategories() = 0;
        virtual std::map<std::string, BufrDataMap> split(const BufrDataMap& dataMap) = 0;
    };
}  //namespace Ingester


