/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "DataProvider.h"

#include <gsl/gsl-lite.hpp>


namespace Ingester {
namespace bufr {
    class QuerySet;

    /// \brief This data provider is used to read standard NCEP files. The complete listing
    ///        of subset table data can be found in the first message of the file, so we only
    ///        need to store this data one time.
    class NcepDataProvider : public DataProvider
    {
     public:
        explicit NcepDataProvider(const std::string& filePath_);

        /// \brief Open the BUFR file with NCEPLIB-bufr
        void open() final;

        /// \brief Gets the variant number for the currently loaded subset.
        size_t variantId() const final;

        /// \brief Returns true if more than one variant have been detected for the currently
        ///        loaded subset.
        bool hasVariants() const final;

     private:
        /// \brief Data for subset table data
        std::shared_ptr<TableData> currentTableData_ = nullptr;

        /// \brief Update the table data for the currently loaded subset.
        /// \param subset The subset string.
        void updateTableData(const std::string& subset) final;

        /// \brief Get the currently valid subset table data
        inline std::shared_ptr<TableData> getTableData() const final { return currentTableData_; }
    };
}  // namespace bufr
}  // namespace Ingester
