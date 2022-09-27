/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */


#pragma once

#include <numeric>
#include <vector>

namespace Ingester {
namespace bufr {

    /// \brief Multiply all the values in a vector together.
    /// \return Scalar value.
    template<typename T>
    T product(const std::vector<T> &vec)
    {
        return std::accumulate(vec.begin(), vec.end(), 1, std::multiplies<T>());
    }

    /// \brief Multiply a range of values in a vector together.
    /// \param begin The beginning of the range.
    /// \param end The end of the range.
    /// \return Scalar value.
    template<typename T>
    T product(typename std::vector<T>::const_iterator begin,
              typename std::vector<T>::const_iterator end)
    {
        T result = 1;
        for (auto i = begin; i < end; ++i)
        {
            result *= *i;
        }

        return result;
    }

    /// \brief Slice a vector into a range of values.
    /// \param begin The beginning of the range.
    /// \param end The end of the range.
    /// \return Sliced vector.
    template<typename T>
    std::vector<T> slice(typename std::vector<T>::const_iterator begin,
                         typename std::vector<T>::const_iterator end)
    {
        std::vector<T> result(end - begin);
        for (auto i = begin; i < end; ++i)
        {
            result[i] = *i;
        }

        return result;
    }

    /// \brief Slice a vector according to a list of indices.
    /// \param vec The vector to slice.
    /// \param indices The indices to slice the vector.
    /// \return Sliced vector.
    template<typename T, typename U>
    std::vector<T> slice(const std::vector<T>& vec, const std::vector<U>& indices)
    {
        std::vector<T> result(indices.size());
        for (size_t i = 0; i < indices.size(); ++i)
        {
            result[i] = vec[indices[i]];
        }

        return result;
    }

    /// \brief Multiple a vector by a scalar.
    /// \param vec The vector to multiply.
    /// \param scalar The scalar to multiply by.
    /// \return Multiplied vector.
    template<typename T, typename U>
    std::vector<T> operator*(const std::vector<T>& vec, const U& scalar)
    {
        std::vector<T> result(vec.size(), 1);
        for (size_t i = 0; i < vec.size(); i++)
        {
            result[i] = vec[i] * static_cast<T>(scalar);
        }

        return result;
    }

    /// \brief Subtract a vector from a scalar.
    /// \param scalar The scalar to subtract from.
    /// \param vec The vector to subtract.
    /// \return Result vector.
    template<typename U, typename T>
    std::vector<T> operator-(const U& scalar, const std::vector<T>& vec)
    {
        std::vector<T> result(vec.size());
        for (size_t i = 0; i < vec.size(); i++)
        {
            result[i] = scalar - vec[i];
        }

        return result;
    }

    /// \brief Subtract a scalar from a vector.
    /// \param vec The vector to subtract from.
    /// \param scalar The scalar to subtract.
    /// \return Result vector.
    template<typename U, typename T>
    std::vector<T> operator-(const std::vector<T>& vec, const U& scalar)
    {
        std::vector<T> result(vec.size());
        for (size_t i = 0; i < vec.size(); i++)
        {
            result[i] = vec[i] - static_cast<T>(scalar);
        }

        return result;
    }

    /// \brief Max value in a vector.
    /// \param vec The vector to find the max value in.
    /// \return Max value.
    template<typename T>
    T max(const std::vector<T>& vec)
    {
        T result = vec[0];
        for (size_t i = 1; i < vec.size(); i++)
        {
            if (vec[i] > result)
            {
                result = vec[i];
            }
        }

        return result;
    }
}  // namespace bufr
}  // namespace Ingester
