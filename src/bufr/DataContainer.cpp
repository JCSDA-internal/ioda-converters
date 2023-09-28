/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */


#include <string>
#include <vector>
#include <ostream>

#include "eckit/exception/Exceptions.h"

#include "DataContainer.h"


namespace Ingester
{
    DataContainer::DataContainer() :
        categoryMap_({})
    {
        makeDataSets();
    }

    DataContainer::DataContainer(const CategoryMap& categoryMap) :
        categoryMap_(categoryMap)
    {
        makeDataSets();
    }

    void DataContainer::add(const std::string& fieldName,
                            const std::shared_ptr<DataObjectBase> data,
                            const SubCategory& categoryId)
    {
        if (hasKey(fieldName, categoryId))
        {
            std::ostringstream errorStr;
            errorStr << "ERROR: Field called " << fieldName << " already exists ";
            errorStr << "for subcategory " << makeSubCategoryStr(categoryId) << std::endl;
            throw eckit::BadParameter(errorStr.str());
        }

        dataSets_.at(categoryId).insert({fieldName, data});
    }

    std::shared_ptr<DataObjectBase> DataContainer::get(const std::string& fieldName,
                                                       const SubCategory& categoryId) const
    {
        if (!hasKey(fieldName, categoryId))
        {
            std::ostringstream errStr;
            errStr << "ERROR: Either field called " << fieldName;
            errStr << " or category " << makeSubCategoryStr(categoryId);
            errStr << " does not exist.";

            throw eckit::BadParameter(errStr.str());
        }

        return dataSets_.at(categoryId).at(fieldName);
    }

#ifdef BUILD_PYTHON_BINDING

    template<typename T>
    std::shared_ptr<DataObjectBase> DataContainer::makeObject(const std::string& fieldName,
                                                              const py::array& pyData,
                                                              T dummy)
    {
        if (!pyData.dtype().is(py::dtype::of<T>()))
        {
            throw std::runtime_error("DataContainer::makeObject: Type mismatch");
        }

        auto dataObj = std::make_shared<DataObject<T>>();
        auto strData = std::vector<T>
            (static_cast<const T*>(pyData.data()),
             static_cast<const T*>(pyData.data()) + pyData.size());

        dataObj->setFieldName(fieldName);
        dataObj->setRawData(std::move(strData));
        dataObj->setDims(std::vector<int> (pyData.shape(), pyData.shape() + pyData.ndim()));
        dataObj->setDimPaths(std::vector<std::string> (pyData.shape().size(), ""));

        return dataObj;
    }

    template<>
    std::shared_ptr<DataObjectBase> DataContainer::makeObject<std::string>(
        const std::string& fieldName,
        const py::array& pyData,
        std::string dummy)
    {
        const auto dtype_str = py::cast<std::string>(py::str(pyData.dtype()));
        if (dtype_str[0] != 'U' && dtype_str[0] != 'S')
        {
            throw std::runtime_error("DataContainer::makeObject: Type mismatch");
        }

        auto dataObj = std::make_shared<DataObject<std::string>>();

        std::vector<std::string> strVec(pyData.size());
        for (size_t i = 0; i < static_cast<size_t>(pyData.size()); i++)
        {
            strVec[i] = py::cast<std::string>(pyData(i));
        }

        dataObj->setFieldName(fieldName);
        dataObj->setRawData(std::move(strVec));
        dataObj->setDims(std::vector<int> (pyData.shape(), pyData.shape() + pyData.ndim()));

        return dataObj;
    }

    void DataContainer::set(const std::string& fieldName,
                            const py::array& pyData,
                            const SubCategory& categoryId)
    {
        std::shared_ptr<DataObjectBase> dataObj;

        py::dtype dt = pyData.dtype();
       // TODO: this line cause RuntimeError: Unable to cast Python instance to C++ type (compile in debug mode for details)
       //  std::string dtype_str = py::cast<std::string>(dt);
        std::string dtype_str =  "aa";
        if (dtype_str[0] == 'U' || dtype_str[0] == 'S')
        {
            dataObj = makeObject<std::string>(fieldName, pyData);
        }
        else if (pyData.dtype().is(py::dtype::of<float>()))
        {
            dataObj = makeObject<float>(fieldName, pyData);
        }
        else if (pyData.dtype().is(py::dtype::of<double>()))
        {
            dataObj = makeObject<double>(fieldName, pyData);
        }
        else if (pyData.dtype().is(py::dtype::of<int>()))
        {
            dataObj = makeObject<int>(fieldName, pyData);
        }
        else if (pyData.dtype().is(py::dtype::of<int64_t>()))
        {
            dataObj = makeObject<int64_t>(fieldName, pyData);
        }
        else
        {
            throw eckit::BadParameter("ERROR: Unsupported data type.");
        }

        if (hasKey(fieldName, categoryId))
        {
            dataSets_.at(categoryId).at(fieldName) = dataObj;
        }
        else
        {
            dataSets_.at(categoryId).insert({fieldName, dataObj});
        }
    }

    py::array DataContainer::getNumpyArray(const std::string& fieldName,
                                           const SubCategory& categoryId) const
    {
        auto dataObj = get(fieldName, categoryId);
        return dataObj->getNumpyArray();
    }
#endif

    std::shared_ptr<DataObjectBase> DataContainer::getGroupByObject(
        const std::string& fieldName,
        const SubCategory& categoryId) const
    {
        if (!hasKey(fieldName, categoryId))
        {
            std::ostringstream errStr;
            errStr << "ERROR: Either field called " << fieldName;
            errStr << " or category " << makeSubCategoryStr(categoryId);
            errStr << " does not exist.";

            throw eckit::BadParameter(errStr.str());
        }

        auto& dataObject = dataSets_.at(categoryId).at(fieldName);
        const auto& groupByFieldName = dataObject->getGroupByFieldName();

        std::shared_ptr<DataObjectBase> groupByObject = dataObject;
        if (!groupByFieldName.empty())
        {
            for (const auto &obj : dataSets_.at(categoryId))
            {
                if (obj.second->getFieldName() == groupByFieldName)
                {
                    groupByObject = obj.second;
                    break;
                }
            }
        }

        return groupByObject;
    }

    bool DataContainer::hasKey(const std::string& fieldName,
                               const SubCategory& categoryId) const
    {
        bool hasKey = false;
        if (dataSets_.find(categoryId) != dataSets_.end() &&
            dataSets_.at(categoryId).find(fieldName) != dataSets_.at(categoryId).end())
        {
            hasKey = true;
        }

        return hasKey;
    }

    size_t DataContainer::size(const SubCategory &categoryId) const
    {
        if (dataSets_.find(categoryId) == dataSets_.end())
        {
            std::ostringstream errStr;
            errStr << "ERROR: Category called " << makeSubCategoryStr(categoryId);
            errStr << " does not exist.";

            throw eckit::BadParameter(errStr.str());
        }

        return  dataSets_.at(categoryId).begin()->second->getDims().at(0);
    }

    std::vector<SubCategory> DataContainer::allSubCategories() const
    {
        std::vector<SubCategory> allCategories;

        for (const auto &dataSetPair : dataSets_)
        {
            allCategories.push_back(dataSetPair.first);
        }

        return allCategories;
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

        size_t numCombos = 1;
        std::vector<size_t> indicies;
        std::vector<size_t> lengths;
        for (const auto& category : categoryMap_)
        {
            indicies.push_back(0);
            lengths.push_back(category.second.size());
            numCombos = numCombos * category.second.size();
        }

        if (!indicies.empty())
        {
            for (size_t idx = 0; idx < numCombos; idx++) {
                size_t catIdx = 0;
                std::vector<std::string> subsets;
                for (const auto &category : categoryMap_) {
                    subsets.push_back(category.second[indicies[catIdx]]);
                    catIdx++;
                }

                dataSets_.insert({subsets, DataSetMap()});
                incIdx(indicies, lengths, 0);
            }
        }
        else
        {
            dataSets_.insert({{}, DataSetMap()});
        }
    }

    std::string DataContainer::makeSubCategoryStr(const SubCategory &categoryId)
    {
        std::ostringstream catStr;

        if (!categoryId.empty())
        {
            for (const auto &subCategory : categoryId)
            {
                catStr << subCategory << "_";
            }
        }
        else
        {
            catStr << "__MAIN__";
        }

        return catStr.str();
    }
}  // namespace Ingester
