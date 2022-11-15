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

    struct TableData
    {
        // Table data;
        std::string subset;
        std::vector<int> isc;
        std::vector<int> link;
        std::vector<int> itp;
        std::vector<int> jmpb;
        std::vector<Typ> typ;
        std::vector<std::string> tag;
        int varientNumber;
    };

    class WmoDataProvider : public DataProvider
    {
     public:
        /// \brief This data provider is used to read standard WMO BUFR files. These BUFR files
        ///        work a bit different from NCEP BUFR files in that there is no master table for
        ///        all the subset tables. Each subset message has a separate copy of its table.
        ///        Whats worse is that the subset identifiers indicated in the message do not
        ///        uniquely identify the data structure of the subsets data. So message subsets
        ///        with the same subset string can have different data structures associated with
        ///        them (they can have different variants). To accommodate this in a way that
        ///        maximizes performance means we need to implement a more elaborate caching scheme
        ///        to store the subset table data.
        WmoDataProvider(const std::string& filePath_,
                        const std::string& tableFilePath_);

        void open() final;

        /// \brief Given the initial BUFR table node idx (see getInode), this function returns
        ///        the node idx for the last BUFR table element for the subset. Valid while
        ///        executing "run".
        /// \param idx BUFR table node index
        inline FortranIdx getIsc(FortranIdx idx) const { return currentTableData_->isc[idx - 1]; }

        /// \brief Given a BUFR table node index, this function returns the next logical node in the
        ///        tree... Valid while executing "run".
        /// \param idx BUFR table node index
        inline FortranIdx getLink(FortranIdx idx) const { return currentTableData_->link[idx - 1]; }

        /// \brief Given a BUFR table node index, this function can give you some type information
        ///        for example a value of 3 is used for strings. Valid while executing "run".
        /// \param idx BUFR table node index
        inline FortranIdx getItp(FortranIdx idx) const { return currentTableData_->itp[idx - 1]; }

        /// \brief Given a BUFR table node index, gives you the node idx for the node that is the
        ///        the next one up in the hierarchy. WARNING: will return 0 for any node at the end
        ///        of any sequence. Valid while executing "run".
        /// \param idx BUFR table node index
        inline FortranIdx getJmpb(FortranIdx idx) const { return currentTableData_->jmpb[idx - 1]; }

        /// \breif Given a BUFR table node index, returns the type (see the Typ enum and maps above)
        ///        Valid while executing "run".
        /// \param idx BUFR table node index
        inline Typ getTyp(FortranIdx idx) const { return currentTableData_->typ[idx - 1]; }

        /// \breif Given a BUFR table node index, returns the tag (name as a human readable string)
        ///        Valid while executing "run".
        /// \param idx BUFR table node index
        inline std::string getTag(FortranIdx idx) const { return currentTableData_->tag[idx - 1]; }

        /// \brief Gets the variant number for the currently loaded subset.
        size_t variantId() const final;

        /// \brief Returns true if more than one variant have been detected for the currently
        ///        loaded subset.
        bool hasVariants() const final;

        /// \brief Initialize the table cache in order to capture all the subset information.
        void initAllTableData() final;

     private:
        typedef std::pair<std::string, size_t> Variant;

        static const int FileUnitTable1 = 13;
        static const int FileUnitTable2 = 14;

        const std::string tableFilePath_;
        std::unordered_map<std::string, std::shared_ptr<TableData>> tableCache_;
        std::shared_ptr<TableData> currentTableData_ = nullptr;

        std::vector<Variant> variants_;
        std::set<Variant> existingVariants;
        std::unordered_map<std::string, size_t> variantCount_;

        /// \brief Update the table data for the currently loaded subset.
        /// \param subset The subset string.
        void updateTableData(const std::string& subset) final;

        /// \brief Deletes the currently loaded data that is stored in the NCEPbufr-lib library.
        void _deleteData() final;
    };

}  // namespace bufr
}  // namespace Ingester
