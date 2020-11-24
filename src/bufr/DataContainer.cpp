/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */


#include <map>
#include <string>
#include <ostream>

#include "Eigen/Dense"
#include "eckit/exception/Exceptions.h"

#include "DataContainer.h"


namespace Ingester
{
    DataContainer::DataContainer(const CategoryMap& categoryMap) :
        categoryMap_(categoryMap)
    {
        makeDataSets();
    }

    void DataContainer::add(const Categories& categoryId,
                            const std::string& fieldName,
                            const std::shared_ptr<DataObject> data)
    {
        if (hasKey(categoryId, fieldName))
        {
            std::ostringstream errorStr;
            errorStr << "ERROR: Field called " << fieldName << " already exists.";
            throw eckit::BadParameter(errorStr.str());
        }

        dataSets_.at(categoryId).insert({fieldName, data});
    }

    std::shared_ptr<DataObject> DataContainer::get(const Categories& categoryId,
                                                   const std::string& fieldName) const
    {
        if (!hasKey(categoryId, fieldName))
        {
            std::ostringstream errorStr;
            errorStr << "ERROR: Field called " << fieldName << " doesn't exists.";
            throw eckit::BadParameter(errorStr.str());
        }

        return dataSets_.at(categoryId).at(fieldName);
    }

    bool DataContainer::hasKey(const Categories& categoryId,
                               const std::string& fieldName) const
    {
        bool hasKey = false;
        if (dataSets_.find(categoryId) != dataSets_.end() &&
            dataSets_.at(categoryId).find(fieldName) != dataSets_.at(categoryId).end())
        {
            hasKey = true;
        }

        return hasKey;
    }

    void DataContainer::makeDataSets()
    {
        std::function<void(std::vector<size_t>&, const std::vector<size_t>&, size_t)> incIdx;
        incIdx = [&incIdx](std::vector<size_t>& indicies,
                    const std::vector<size_t>& lengths,
                    size_t idx)
        {
            if (indicies[idx] + 1 >= lengths[idx])
            {
                if (idx + 1 < indicies.size())
                {
                    indicies[idx] = 0;
                    incIdx(indicies, lengths, idx + 1);
                }
            }
            else
            {
                indicies[idx]++;
            }
        };

        size_t totalCnt = 1;
        std::vector<size_t> indicies;
        std::vector<size_t> lengths;
        for (const auto& category : categoryMap_)
        {
            indicies.push_back(0);
            lengths.push_back(category.second.size());
            totalCnt = totalCnt * category.second.size();
        }

        for (size_t idx = 0; idx < totalCnt; idx++)
        {
            size_t catIdx = 0;
            std::vector<std::string> subsets;
            for (const auto& category : categoryMap_)
            {
                subsets.push_back(category.second[indicies[catIdx]]);
                catIdx++;
            }

            categoryIdxs_.push_back(indicies);
            dataSets_.insert({subsets, DataSetMap()});
            incIdx(indicies, lengths, 0);
        }

        for (const auto& s : dataSets_)
        {
            std::cout << s.first[0] << std::endl;
        }
    }
}  // namespace Ingester
