/*
 * (C) Copyright 2018-2019 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef OOPS_UTIL_INTSETPARSER_H_
#define OOPS_UTIL_INTSETPARSER_H_

#include <set>
#include <string>

namespace oops {
    std::set<int> parseIntSet(const std::string &);

    template<typename TT>
    bool contains(const std::set<TT> & set, const TT & elem) {
        return set.find(elem) != set.end();
    }
}  // namespace oops

#endif  // OOPS_UTIL_INTSETPARSER_H_
