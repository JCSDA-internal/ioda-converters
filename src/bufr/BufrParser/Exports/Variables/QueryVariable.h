/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "Variable.h"
#include "IngesterTypes.h"
#include "DataObject/ArrayDataObject.h"
#include "Transforms/Transform.h"


namespace Ingester
{
    /// \brief Exports parsed data associated with a mnemonic (ex: "CLAT")
    class QueryVariable final : public Variable
    {
     public:
        explicit QueryVariable(const std::string& exportName,
                               const std::string& query,
                               const std::string& groupByField,
                               const Transforms& transforms);

        ~QueryVariable() final = default;

        /// \brief Gets the requested data, applies transforms, and returns the requested data
        /// \param map BufrDataMap that contains the parsed data for each mnemonic
        std::shared_ptr<DataObject> exportData(const BufrDataMap& map) final;

        /// \brief Get a list of queries for this variable
        QueryList makeQueryList() const final;

     private:
        /// \brief The query of interest
        std::string query_;

        /// \brief The for field of interest
        std::string groupByField_;

        /// \brief Collection of transforms to apply to the data during export
        Transforms transforms_;

        /// \brief Apply the transforms
        /// \param data Eigen Array data to apply the transform to.
        void applyTransforms(IngesterArray& data);
    };
}  // namespace Ingester
