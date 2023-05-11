/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <memory>

#include "eckit/config/LocalConfiguration.h"

#include "Variable.h"


namespace Ingester
{
    /// \brief Exports parsed data as SensorScanPosition  using speciefied Mnemonics
    class SensorScanPositionVariable final : public Variable
    {
     public:
        SensorScanPositionVariable() = delete;
        SensorScanPositionVariable(const std::string& exportName,
                                 const std::string& groupByField,
                                 const eckit::LocalConfiguration& conf);

        ~SensorScanPositionVariable() final = default;

        /// \brief Get the configured mnemonics and turn them into SensorScanPosition
        /// \param map BufrDataMap that contains the parsed data for each mnemonic
        std::shared_ptr<DataObjectBase> exportData(const BufrDataMap& map) final;

        /// \brief Get a list of queries for this variable
        QueryList makeQueryList() const final;

     private:
        /// \brief makes sure the bufr data map has all the required keys.
        void checkKeys(const BufrDataMap& map);

        /// \brief get the export key string
        std::string getExportKey(const std::string& name) const;
    };
}  // namespace Ingester
