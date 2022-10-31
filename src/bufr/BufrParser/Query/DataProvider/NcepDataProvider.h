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

        void open() final;

        // Getters to get the raw data by idx. Since fortran indices are 1-based,
        // we need to subtract 1 to get the correct c style index.
        inline FortranIdx getIsc(FortranIdx idx) const { return isc_[idx - 1]; }
        inline FortranIdx getLink(FortranIdx idx) const { return link_[idx - 1]; }
        inline FortranIdx getItp(FortranIdx idx) const { return itp_[idx - 1]; }
        inline FortranIdx getJmpb(FortranIdx idx) const { return jmpb_[idx - 1]; }
        inline Typ getTyp(FortranIdx idx) const { return typ_[idx - 1]; }
        inline std::string getTag(FortranIdx idx) const { return tag_[idx - 1]; }

        size_t variantId() const final;
        bool hasVariants() const final;

     private:
        // Table data;
        gsl::span<const int> isc_;
        gsl::span<const int> link_;
        gsl::span<const int> itp_;
        gsl::span<const int> jmpb_;
        std::vector<Typ> typ_;
        std::vector<std::string> tag_;

        void updateTableData(const std::string& subset) final;
        void _deleteData() final;
    };
}  // namespace bufr
}  // namespace Ingester
