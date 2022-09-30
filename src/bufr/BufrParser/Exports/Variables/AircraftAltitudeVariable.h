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
    /// \brief Exports parsed data as aircraftAltitudes using specified Mnemonics
    class AircraftAltitudeVariable final : public Variable
    {
     public:
        AircraftAltitudeVariable() = delete;
        AircraftAltitudeVariable(const std::string& exportName,
                         const std::string& groupByField,
                         const eckit::Configuration& conf);
        ~AircraftAltitudeVariable() final = default;

        /// \brief Get the configured mnemonics and turn them into AircraftAltitude 
        /// \param map BufrDataMap that contains the parsed data for each mnemonic
        std::shared_ptr<DataObjectBase> exportData(const BufrDataMap& map) final;

        /// \brief Get a list of queries for this variable
        QueryList makeQueryList() const final;

     private:
        /// \brief Query for latitude (required)
        const std::string latitudeQuery_;

        /// \brief Query for pressure (PRLC) (optional)
        std::string pressureQuery_;

        /// \brief Query for indicated Aircraft Altitude (IALR) (optional)
        std::string aircraftIndicatedAltitudeQuery_;

        /// \brief Query for Pressure Altitude Relative to MSL (PSAL) (optional)
        std::string pressureAltitudeRelativeToMeanSeaLevelQuery_;

        /// \brief Query for Flight Level (FLVL) (optional)
        std::string flightLevelQuery_;

        /// \brief Query for height (HEIT) (optional)
        std::string heightQuery_;

        /// \brief Query for heightOrAltitude (HMSL) (optional)
        std::string heightOrAltitudeQuery_;

        /// \brief Query for Flight Level ST (FLVLST) (optional)
        std::string flightLevelSTQuery_;

        /// \brief For field
        std::string groupByField_;

        /// \brief makes sure the bufr data map has all the required keys.
        void checkKeys(const BufrDataMap& map);

        /// \brief get the export key string
        std::string getExportKey(const char* name) const;
    };
}  // namespace Ingester
