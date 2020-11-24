//
// Created by Ronald McLaren on 11/11/20.
//

#pragma once

#include "Split.h"

#include <string>
#include <vector>
#include <map>


namespace Ingester
{
    class CategorySplit : public Split
    {
     public:
        typedef  std::map<size_t, std::string> NameMap;

        CategorySplit(const std::string& mnemonic, const NameMap& map);

        inline std::string getMnemonic() { return mnemonic_; }

        std::vector<std::string> subCategories() final;
        std::map<std::string, BufrDataMap> split(const BufrDataMap& dataMap) final;

    private:
        const NameMap nameMap_;
        const std::string mnemonic_;
    };
}


