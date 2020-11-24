//
// Created by Ronald McLaren on 11/11/20.
//

#include "RangeFilter.h"

namespace Ingester
{
    RangeFilter::RangeFilter(std::string mnemonic, std::vector<float> extents) :
      mnemonic_(mnemonic),
      extents_(extents)
    {
    }

    void RangeFilter::apply(IngesterArray& array)
    {

    }
}  // namespace Ingester