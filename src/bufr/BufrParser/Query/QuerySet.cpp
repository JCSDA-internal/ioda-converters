//
// Created by rmclaren on 1/30/22.
//

#include "QuerySet.h"


namespace Ingester {
namespace bufr {

    std::vector<std::string> QuerySet::names() const
    {
        std::vector<std::string> names;
        for (auto const& query : queryList_)
        {
            names.push_back(query.first);
        }

        return names;
    }

}  // namespace bufr
}  // namespace Ingester