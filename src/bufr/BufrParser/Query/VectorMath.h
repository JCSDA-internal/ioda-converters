//
// Created by rmclaren on 2/1/22.
//

#pragma once

#include <vector>

namespace Ingester
{
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

    template<typename T>
    std::vector<T> slice(std::vector<T> vec, std::vector<size_t> indices)
    {
        std::vector<T> result;
        for (auto i : indices)
        {
            result.push_back(vec[i]);
        }

        return result;
    }

    template<typename T, typename U>
    std::vector<T> operator*(std::vector<T> vec, U scalar)
    {
        std::vector<T> result(vec.size());
        for (size_t i = 0; i < vec.size(); i++)
        {
            result[i] *= static_cast<T>(scalar);
        }

        return result;
    }

    template<typename U, typename T>
    std::vector<T> operator-(U scalar, std::vector<T> vec)
    {
        std::vector<T> result(vec.size());
        for (size_t i = 0; i < vec.size(); i++)
        {
            result[i] = scalar - vec[i];
        }

        return result;
    }
}
