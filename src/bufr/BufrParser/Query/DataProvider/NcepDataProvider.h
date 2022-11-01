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

    class NcepDataProvider : public DataProvider
    {
     public:
        explicit NcepDataProvider(const std::string& filePath_);

        /// \brief Open the BUFR file with NCEPLIB-bufr
        void open() final;

        /// \brief Given the initial BUFR table node idx (see getInode), this function returns
        ///        the node idx for the last BUFR table element for the subset.
        /// \param idx BUFR table node index
        inline FortranIdx getIsc(FortranIdx idx) const { return isc_[idx - 1]; }

        /// \brief Given a BUFR table node index, this function returns the next logical node in the
        ///        tree...
        /// \param idx BUFR table node index
        inline FortranIdx getLink(FortranIdx idx) const { return link_[idx - 1]; }

        /// \brief Given a BUFR table node index, this function can give you some type information
        ///        for example a value of 3 is used for strings.
        /// \param idx BUFR table node index
        inline FortranIdx getItp(FortranIdx idx) const { return itp_[idx - 1]; }

        /// \brief Given a BUFR table node index, gives you the node idx for the node that is the
        ///        the next one up in the hierarchy. WARNING: will return 0 for any node at the end
        ///        of any sequence.
        /// \param idx BUFR table node index
        inline FortranIdx getJmpb(FortranIdx idx) const { return jmpb_[idx - 1]; }

        /// \breif Given a BUFR table node index, returns the type (see the Typ enum and maps above)
        /// \param idx BUFR table node index
        inline Typ getTyp(FortranIdx idx) const { return typ_[idx - 1]; }

        /// \breif Given a BUFR table node index, returns the tag (name as a human readable string)
        /// \param idx BUFR table node index
        inline std::string getTag(FortranIdx idx) const { return tag_[idx - 1]; }

        /// \brief Gets the variant number for the currently loaded subset.
        size_t variantId() const final;

        /// \brief Returns true if more than one variant have been detected for the currently
        ///        loaded subset.
        bool hasVariants() const final;

     private:
        // Table data;
        gsl::span<const int> isc_;
        gsl::span<const int> link_;
        gsl::span<const int> itp_;
        gsl::span<const int> jmpb_;
        std::vector<Typ> typ_;
        std::vector<std::string> tag_;

        /// \brief Update the table data for the currently loaded subset.
        /// \param subset The subset string.
        void updateTableData(const std::string& subset) final;

        /// \brief Deletes the currently loaded data that is stored in the NCEPbufr-lib library.
        void _deleteData() final;
    };
}  // namespace bufr
}  // namespace Ingester
