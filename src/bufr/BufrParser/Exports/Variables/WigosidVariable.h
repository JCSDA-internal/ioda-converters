/*
 * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
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
    /// \brief Exports parsed data as wigosid using speciefied Mnemonics
    class WigosidVariable final : public Variable
    {
     public:
        WigosidVariable() = delete;
        WigosidVariable(const std::string& exportName,
                         const std::string& groupByField,
                         const eckit::LocalConfiguration& conf);
        ~WigosidVariable() final = default;

        /// \brief Get the configured mnemonics and turn them into wigosid strings
        /// \param map BufrDataMap that contains the parsed data for each mnemonic
        std::shared_ptr<DataObjectBase> exportData(const BufrDataMap& map) final;

        /// \brief Get a list of queries for this variable
        QueryList makeQueryList() const final;

     private:
        /// \brief Query for wgosids
        const std::string wgosidsQuery_;

        /// \brief Query for wgosisid
        const std::string wgosisidQuery_;

        /// \brief Query for wgosisnm
        const std::string wgosisnmQuery_;

        /// \brief Query for wgoslid
        const std::string wgoslidQuery_;

        /// \brief makes sure the bufr data map has all the required keys.
        void checkKeys(const BufrDataMap& map);

        /// \brief get the export key string
        std::string getExportKey(const char* name) const;
    };
}  // namespace Ingester
