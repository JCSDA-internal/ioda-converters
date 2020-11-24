//
// Created by Ronald McLaren on 11/11/20.
//

#pragma once

#include "Filter.h"

#include <string>
#include <vector>


namespace Ingester
{
    class RangeFilter : public Filter
    {
     public:
        RangeFilter(std::string mnemonic, std::vector<float> extents);

        void apply(IngesterArray& array) final;

     private:
         const std::string mnemonic_;
         const std::vector<float> extents_;
    };
}  // namespace Ingester
