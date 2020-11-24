//
// Created by Ronald McLaren on 11/11/20.
//

#pragma once

#include <memory>
#include <map>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "Filters/Filter.h"
#include "Splits/Split.h"
#include "Variables/Variable.h"


namespace Ingester
{
    class Export
    {
     public:
        typedef std::map<std::string, std::shared_ptr<Split>> Splits;
        typedef std::map<std::string, std::shared_ptr<Variable>> Variables;
        typedef std::vector<std::shared_ptr<Filter>> Filters;

        explicit Export(const eckit::Configuration &conf);

        inline Splits getSplits() const { return splits_; }
        inline Variables getVariables() const { return variables_; }
        inline Filters getFilters() const { return filters_; }

     private:
        Splits splits_;
        Variables  variables_;
        Filters filters_;

        void addVariables(const eckit::Configuration &conf);
        void addSplits(const eckit::Configuration &conf);
        void addFilters(const eckit::Configuration &conf);
    };
}  // namespace Ingester
