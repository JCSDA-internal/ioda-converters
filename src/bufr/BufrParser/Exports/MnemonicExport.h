/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <memory>

#include "eckit/config/LocalConfiguration.h"

#include "Export.h"
#include "EncoderTypes.h"
#include "DataObject/ArrayDataObject.h"
#include "Transforms/Transform.h"


namespace BufrParser
{
    /// \brief Exports parsed data associated with a mnemonic (ex: "CLAT")
    class MnemonicExport final : public Export
    {
     public:
        explicit MnemonicExport(std::string mnemonicStr, Transforms transforms);
        ~MnemonicExport() final = default;

        /// \brief Gets the requested data, applies transforms, and returns the requested data
        /// \param map BufrDataMap that contains the parsed data for each mnemonic
        std::shared_ptr<IodaEncoder::DataObject> exportData(const BufrDataMap& map) final;

     private:
        /// \brief The BUFR mnemonic of interest
        std::string mnemonic_;

        /// \brief Collection of transforms to apply to the data during export
        Transforms transforms_;

        /// \brief Apply the transforms
        /// \param data Eigen Array data to apply the transform to.
        void applyTransforms(IodaEncoder::EncoderArray& data);
    };
}  // namespace BufrParser
