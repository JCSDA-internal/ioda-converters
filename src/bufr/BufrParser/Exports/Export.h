/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "Filters/Filter.h"
#include "Splits/Split.h"
#include "Variables/Variable.h"


namespace Ingester
{
    /// \brief Uses configuration to determine all the things needed to be done on export.
    class Export
    {
     public:
        typedef std::vector<std::shared_ptr<Split>> Splits;
        typedef std::vector<std::shared_ptr<Variable>> Variables;
        typedef std::vector<std::shared_ptr<Filter>> Filters;

        /// \brief Constructor
        /// \param conf Config data/
        explicit Export(const eckit::Configuration &conf);

        // Getters
        inline Splits getSplits() const { return splits_; }
        inline Variables getVariables() const { return variables_; }
        inline Filters getFilters() const { return filters_; }

     private:
        Splits splits_;
        Variables  variables_;
        Filters filters_;

        /// \brief Create Variables exports from config.
        void addVariables(const eckit::Configuration &conf,
                          const std::string& groupByVariable = "");

        /// \brief Create Splits exports from config.
        void addSplits(const eckit::Configuration &conf);

        /// \brief Create Filters exports from config.
        void addFilters(const eckit::Configuration &conf);
    };
}  // namespace Ingester
