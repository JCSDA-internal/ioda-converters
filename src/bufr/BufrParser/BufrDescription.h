/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "Exports/Export.h"


namespace Ingester
{
    class BufrMnemonicSet;
    class Variable;

    /// \brief Description of the data to be read from a BUFR file and how to expose that data to
    /// the outside world.
    class BufrDescription
    {
     public:
        explicit BufrDescription(const eckit::Configuration &conf);

        /// \brief Add a BufrMnemonicSet to the description.
        /// \param mnemonicSet BufrMnemonicSet to add
        void addMnemonicSet(const BufrMnemonicSet& mnemonicSet);

        // Setters
        inline void setFilepath(const std::string& filepath) { filepath_ = filepath; }
        inline void setIsWmoFormat(bool isWmoFormat) { isWmoFormat_ = isWmoFormat; }
        inline void setTablepath(const std::string& tablepath) { tablepath_ = tablepath; }
        inline void setExport(const Export& newExport) { export_ = newExport; }

        // Getters
        inline std::string filepath() const { return filepath_; }
        inline bool isWmoFormat() const { return isWmoFormat_; }
        inline std::string tablepath() const { return tablepath_; }
        inline Export getExport() const { return export_; }

     private:
        /// \brief Specifies the relative path to the BUFR file to read.
        std::string filepath_;

        /// \brief Does the bufr file use the standard BUFR format (no NCEP table data in file).
        bool isWmoFormat_;

        /// \brief Specifies the relative path to the master tables (applies to std BUFR files).
        std::string tablepath_;

        /// \brief Map of export strings to Variable classes.
        Export export_;
    };
}  // namespace Ingester
