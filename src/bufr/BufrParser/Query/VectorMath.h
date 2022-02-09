//
// Created by rmclaren on 2/1/22.
//

#pragma once

#include <vector>

namespace Ingester {
namespace bufr {
    template<typename T>
    T product(std::vector<T> vec)
    {
        T result = 1;
        for (auto i : vec)
        {
            result *= i;
        }

        return result;
    }

    template<typename T>
    T product(typename std::vector<T>::const_iterator begin,
              typename std::vector<T>::const_iterator end)
    {
        T result = 1;
        for (auto i = begin; i != end; ++i)
        {
            result *= *i;
        }

        return result;
    }

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
}  // namespace bufr
}  // namespace Ingester
