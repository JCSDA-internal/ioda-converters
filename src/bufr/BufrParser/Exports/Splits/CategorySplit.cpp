//
// Created by Ronald McLaren on 11/11/20.
//

#include "CategorySplit.h"

#include <ostream>

#include "eckit/exception/Exceptions.h"

#include "EigenExt.h"


namespace Ingester
{
    CategorySplit::CategorySplit(const std::string& mnemonic, const NameMap& nameMap) :
      nameMap_(nameMap),
      mnemonic_(mnemonic)
    {
    }

    std::vector<std::string> CategorySplit::subCategories()
    {
        std::vector<std::string> categories;
        for (const auto& name : nameMap_)
        {
            categories.push_back(name.second);
        }

        return categories;
    }

    std::map<std::string, BufrDataMap> CategorySplit::split(const BufrDataMap &dataMap)
    {
        std::map<std::string, BufrDataMap> dataMaps;

        const IngesterArray& mnemonicArr = dataMap.at(mnemonic_);

        for (const auto& mapPair : nameMap_)
        {
            //Find matching rows
            std::vector<int> indexVec;
            for (int rowIdx = 0;
                 rowIdx < static_cast<int>(dataMap.at(mnemonic_).rows());
                 rowIdx++)
            {
                if (abs(mnemonicArr.row(rowIdx)[0] - static_cast<float>(mapPair.first)) < .0001)
                {
                    indexVec.push_back(rowIdx);
                }
            }

            //Make new data map
            BufrDataMap newDataMap;
            for (const auto& dataPair : dataMap)
            {
                auto columns = Eigen::VectorXi(1);

                for (size_t colIdx = 0;
                     colIdx < static_cast<size_t>(dataPair.second.cols());
                     colIdx++)
                {
                    columns << colIdx;
                }

                auto indexArr = VectorXi::Map(indexVec.data(), indexVec.size());
                const auto newArr = indexing(dataPair.second, indexArr, columns);
                newDataMap.insert({dataPair.first, newArr});
            }

            dataMaps.insert({mapPair.second, newDataMap});
        }

        return dataMaps;
    }
}