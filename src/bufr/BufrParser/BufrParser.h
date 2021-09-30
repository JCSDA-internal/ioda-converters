/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "Eigen/Dense"

#include "eckit/config/LocalConfiguration.h"

#include "Parser.h"
#include "BufrTypes.h"
#include "BufrDescription.h"


namespace Ingester
{
    class BufrMnemonicSet;
    class DataContainer;

    /// \brief Uses a BufrDescription and helper classes to parse the contents of a BUFR file.
    class BufrParser final : public Parser
    {
     public:
        explicit BufrParser(const BufrDescription& description);
        explicit BufrParser(const eckit::Configuration& conf);

        ~BufrParser();

        /// \brief Uses the provided description to parse the buffer file.
        /// \param maxMsgsToParse Messages to parse (0 for everything)
        std::shared_ptr<DataContainer> parse(const size_t maxMsgsToParse = 0) final;

        /// \brief Start over from beginning of the BUFR file
        void reset() final;

     private:
        typedef std::map<std::vector<std::string>, BufrDataMap> CatDataMap;

        /// \brief The description the defines what to parse from the BUFR file
        BufrDescription description_;

        /// \brief The Fortran file ID to an open BUFR file (0 when no file open)
        unsigned int fortranFileId_;

        /// \brief The Fortran file ID to an open BUFR file (0 when no file open)
        unsigned int table1FileId_;

        /// \brief The Fortran file ID to an open BUFR file (0 when no file open)
        unsigned int table2FileId_;

        /// \brief Exports collected data into a DataContainer
        /// \param srcData Data to export
        std::shared_ptr<DataContainer> exportData(const BufrDataMap& srcData);

        /// \brief Function responsible for dividing the data into subcategories.
        /// \details This function is intended to be called over and over for each specified Split
        ///          object, sub-splitting the data given into all the possible subcategories.
        /// \param splitMaps Pre-split map of data.
        /// \param split Object that knows how to split data.
        CatDataMap splitData(CatDataMap& splitMaps, Split& split);

        /// \brief Opens a BUFR file using the Fortran BUFR interface.
        /// \param filepath Path to bufr file.
        /// \param isWmoFormat _optional_ Bufr file is in the standard format.
        /// \param tablepath _optional_ Path to WMO master tables (needed for standard bufr files).
        void
        openBufrFile(const std::string& filepath, bool isWmoFormat, const std::string& tablepath);

        /// \brief Closes the open BUFR file.
        void closeBufrFile();

        /// \brief Convenience method to print the Categorical data map to stdout.
        void printMap(const CatDataMap& map);
    };
}  // namespace Ingester
