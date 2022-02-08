//
// Created by rmclaren on 1/30/22.
//

#pragma once

#include <vector>
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

        void add(const std::string& name, const std::string& query) { queryList_.push_back({name, query}); };
        int size() const { return queryList_.size(); };
        std::string nameAt(size_t idx) const { return queryList_.at(idx).first; };
        std::string queryAt(size_t idx) const { return queryList_.at(idx).second; };
        std::vector<std::string> names() const;

    private:
        std::vector<std::pair<std::string, std::string>> queryList_;
    };
}  // namespace bufr
}  // namespace Ingester
