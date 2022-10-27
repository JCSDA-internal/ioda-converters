//
// Created by Ronald McLaren on 10/26/22.
//

#pragma once

#include <algorithm>
#include <string>


namespace Ingester {
namespace bufr {
    struct SubsetVariant
    {
        std::string subset;
        size_t variantId;
        bool otherVariantsExist = false;

        std::string str() const
        {
            if (otherVariantsExist)
            {
                return subset + "[" + std::to_string(variantId) + "]";
            }
            else
            {
                return subset;
            }
        }

        bool operator< (const SubsetVariant& right) const
        {
            return str() < right.str();
        }

        bool operator== (const SubsetVariant& right) const
        {
            return (subset == right.subset) && (variantId == right.variantId);
        }
    };
}  // namespace bufr
}  // namespace Ingester

// Implement a version of the hash function for ths Subset Variant class so that we may use it as a
// unordered map keys.
namespace std
{
    template<>
    struct hash<Ingester::bufr::SubsetVariant>
    {
        std::size_t operator()(const Ingester::bufr::SubsetVariant &k) const
        {
            return std::hash<string>()(k.subset) ^ (std::hash<size_t>()(k.variantId) << 1);
        }
    };
}  // namespace std
