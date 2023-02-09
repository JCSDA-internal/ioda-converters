/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "DataProvider.h"

#include <string>
#include <unordered_map>
#include <memory>
#include <gsl/gsl-lite.hpp>

#include "../QuerySet.h"


namespace Ingester {
namespace bufr {

    /// \brief This data provider is used to read standard WMO BUFR files. These BUFR files
    ///        work a bit different from NCEP BUFR files in that there is no master table for
    ///        all the subset tables. Each subset message has a separate copy of its table.
    ///        Whats worse is that the subset identifiers indicated in the message do not
    ///        uniquely identify the data structure of the subsets data. So message subsets
    ///        with the same subset string can have different data structures associated with
    ///        them (they can have different variants). To accommodate this in a way that
    ///        maximizes performance means we need to implement a more elaborate caching scheme
    ///        to store the subset table data.
    class WmoDataProvider : public DataProvider
    {
     public:
        WmoDataProvider(const std::string& filePath_,
                        const std::string& tableFilePath_);

        /// \brief Open the BUFR file with NCEPLIB-bufr
        void open() final;

        /// \brief Gets the variant number for the currently loaded subset.
        size_t variantId() const final;

        /// \brief Returns true if more than one variant have been detected for the currently
        ///        loaded subset.
        bool hasVariants() const final;

        /// \brief Initialize the table cache in order to capture all the subset information.
        void initAllTableData() final;

     private:
        static const int FileUnitTable1 = 13;
        static const int FileUnitTable2 = 14;

        const std::string tableFilePath_;
        std::unordered_map<std::string, std::shared_ptr<TableData>> tableCache_;
        std::shared_ptr<TableData> currentTableData_ = nullptr;
        std::unordered_map<std::string, size_t> variantCount_;

        /// \brief Update the table data for the currently loaded subset.
        /// \param subset The subset string.
        void updateTable(const std::string& subset) final;

        /// \brief Get the currently valid subset table data
        inline std::shared_ptr<TableData> getTableData() const final { return currentTableData_; };
    };
}  // namespace bufr
}  // namespace Ingester
