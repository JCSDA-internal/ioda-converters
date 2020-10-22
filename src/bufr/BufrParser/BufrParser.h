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

#include "Eigen/Dense"

#include "BufrTypes.h"
#include "BufrDescription.h"


namespace Ingester
{
    class BufrMnemonicSet;
    class DataContainer;

    /// \brief Uses a BufrDescription and helper classes to parse the contents of a BUFR file.
    class BufrParser
    {
     public:
        explicit BufrParser(BufrDescription& description);
        ~BufrParser();

        /// \brief Uses the provided description to parse the buffer file.
        /// \param maxMsgsToParse Messages to parse (0 for everything)
        std::shared_ptr<DataContainer> parse(const size_t maxMsgsToParse = 0);

        /// \brief Start over from beginning of the BUFR file
        void reset();

     private:
        /// \brief The description the defines what to parse from the BUFR file
        BufrDescription description_;

        /// \brief The Fortran file ID to an open BUFR file (0 when no file open)
        unsigned int fortranFileId_;

        /// \brief Exports collected data into a DataContainer
        std::shared_ptr<DataContainer> exportData(const BufrDataMap& sourceData);

        /// \brief Opens a BUFR file using the Fortran BUFR interface.
        void openBufrFile(const std::string& filepath);

        /// \brief Closes the open BUFR file.
        void closeBufrFile();
    };
}  // namespace Ingester
