/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <Eigen/Dense>
#include <string>
#include <vector>


/// Collection of template methods that are used to slice array and vector data.

/// \brief Slice eigen array using indicies of eigen arrays.
template<class EigenType, class EigenIdxType>
EigenType rowSlice(const EigenType& arr, const EigenIdxType& idxVec)
{
    EigenType result(idxVec.rows(), arr.cols());

    for (size_t rowIdx = 0; rowIdx < idxVec.rows(); rowIdx++)
    {
        result.row(rowIdx) = arr.row(idxVec.at(rowIdx)(0));
    }

    return result;
}

/// \brief Slice eigen array using std::vector of indicies.
template<class EigenType, typename IdxType>
EigenType rowSlice(const EigenType& arr, const std::vector<IdxType>& idxVec)
{
    EigenType result(idxVec.size(), arr.cols());

    for (size_t rowIdx = 0; rowIdx < idxVec.size(); rowIdx++)
    {
        result.row(rowIdx) = arr.row(static_cast<Eigen::Index>(idxVec[rowIdx]));
    }

    return result;
}

/// \brief Slice string vector using indicies of eigen arrays.
template<class EigenIdxType>
std::vector<std::string> rowSlice(const std::vector<std::string>& arr,
                                  const EigenIdxType& idxVec)
{
    std::vector<std::string> result;
    result.resize(idxVec.rows());

    for (Eigen::Index rowIdx = 0; rowIdx < idxVec.rows(); rowIdx++)
    {
        result[rowIdx] = arr[idxVec.at(rowIdx)(0)];
    }

    return result;
}

/// \brief Slice string vector using std::vector of indicies.
template<typename IdxType>
std::vector<std::string> rowSlice(const std::vector<std::string>& arr,
                                  const std::vector<IdxType>& idxVec)
{
    std::vector<std::string> result;
    result.resize(idxVec.size());

    for (Eigen::Index rowIdx = 0; rowIdx < idxVec.size(); rowIdx++)
    {
        result[rowIdx] = arr[static_cast<Eigen::Index>(idxVec[rowIdx])];
    }

    return result;
}

