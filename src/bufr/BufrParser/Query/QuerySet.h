//
// Created by rmclaren on 1/30/22.
//

#pragma once

#include <string>
#include <map>

namespace Ingester {
namespace bufr
{
    class QuerySet
    {
    public:
        QuerySet() = default;
        ~QuerySet() = default;

        void add(const std::string& key, const std::string& value) { queryMap_[key] = value; };
        int count() const { return queryMap_.size(); };


    private:
        std::map<std::string, std::string> queryMap_;
    };
}  // namespace bufr
}  // namespace Ingester
